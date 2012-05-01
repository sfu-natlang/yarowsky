(*
	Decision list implementation.
*)

open Utils

type rule = float * Data.Feature.t * Data.Label.t

type t = {
	labels : Data.Label.t list;
	features : Data.Feature.t list;
	rules : rule list;
	feat_table : (int * float * Data.Label.t) Data.Feature.Map.t;
	pair_table : ((int * float) option) Data.Table.t;
}

let of_rules labels features rules =
	let sorted = List.stable_sort (fun (s1, _, _) (s2, _, _) -> sign (s2 -. s1)) rules in
	let feat_table, _ = List.fold_left begin fun (m, i) (s, f, l) -> 
			let m1 =
				if try let _, s1, _ = Data.Feature.Map.find f m in s > s1 with Not_found -> true then Data.Feature.Map.add f (i, s, l) m
				else m in
				(m1, i + 1)
		end (Data.Feature.Map.empty, 0) sorted in
	let pair_table = Data.Table.make labels features None in
		List.iteri (fun i (s, f, l) -> Data.Table.set l f (Some (i, s)) pair_table) sorted;
		{ labels; features; rules; feat_table; pair_table }

let length dl =
	List.length dl.rules

let iter f dl =
	List.iter f dl.rules

let fold f a dl =
	List.fold_left f a dl.rules

let load lookup ic =
	let space = Str.regexp "[ \t]+" in
	let rules = lines_to_list begin fun line ->
			match Str.split space line with
			| [score; feat; label] -> float_of_string score, Data.make_feature lookup feat, Data.make_label lookup label
			| _ -> assert false
		end ic in
	let labels = Data.uniq_labels (List.rev_map (fun (_, _, l) -> l) rules) in
	let features = Data.uniq_features (List.rev_map (fun (_, f, _) -> f) rules) in
		of_rules labels features rules

let save lookup dl oc =
	List.iter begin fun (score, feat, label) ->
		Printf.fprintf oc "%f %s %s\n" score (Data.string_of_feature lookup feat) (Data.string_of_label lookup label)
	end dl.rules

let filter p dl =
	of_rules dl.labels dl.features (List.filter p dl.rules)

let merge labels features dls =
	let table = Data.Table.make labels features neg_infinity in
		List.iter begin fun (score, feat, label) ->
			if score > Data.Table.get label feat table then Data.Table.set label feat score table
		end (List.concat (List.map (fun dl -> dl.rules) dls));
		let uniq_rules = Iter.to_list begin fun add ->
				Data.Table.iteri begin fun label feat score ->
					if score > 0.0 then add (score, feat, label)
				end table
			end in
			of_rules labels features uniq_rules

let apply_score dl example =
	try
		let f = argmini (fun f -> try let i, _, _ = Data.Feature.Map.find f dl.feat_table in i with Not_found -> max_int) example in
		let _, s, l = Data.Feature.Map.find f dl.feat_table in
			Some (s, l)
	with Not_found -> None

let apply dl example = match apply_score dl example with Some (_, l) -> Some l | None -> None

let apply_sum_score ?(allow_zero=false) dl example =
	let choices = List.map begin fun l ->
		l, sumf (fun f -> try (match Data.Table.get l f dl.pair_table with Some (_, s) -> s | None -> 0.0) with Not_found -> 0.0) example
	end dl.labels in
	let sorted = List.stable_sort (fun (_, s1) (_, s2) -> sign (s2 -. s1)) choices in
		match sorted with
		| (l, s)::_ when allow_zero || s > 0.0 -> Some (s, l)
		| _ -> None

let apply_sum dl example = match apply_sum_score dl example with Some (_, l) -> Some l | None -> None

let score dl label feat =
	match Data.Table.get label feat dl.pair_table with
	| Some result -> result
	| None -> raise Not_found

let feat_mem dl feat = Data.Feature.Map.mem feat dl.feat_table

(*
	Specific decision list matching Collins & Singer 1999.
*)
module Collins_singer =
struct
	let counts labels features data labelling smoothing =
		let feat_counts = ref Data.Feature.Map.empty in
		let feat_counts_unlabelled = ref Data.Feature.Map.empty in
		let pair_counts = Data.Table.make labels features 0 in
		let count_feat_unlabelled feat = try Data.Feature.Map.find feat !feat_counts_unlabelled with Not_found -> 0 in
		let count_feat feat = try Data.Feature.Map.find feat !feat_counts with Not_found -> 0 in
		let count_pair feat label = try Data.Table.get label feat pair_counts with Not_found -> 0 in
			List.iter2 begin fun example label ->
				match label with
				| Some label ->
					List.iter begin fun feat ->
						feat_counts := Data.Feature.Map.add feat (1 + count_feat feat) !feat_counts;
						Data.Table.add_val label feat 1 pair_counts
					end example
				| None ->
					List.iter begin fun feat ->
						feat_counts_unlabelled := Data.Feature.Map.add feat (1 + count_feat_unlabelled feat) !feat_counts_unlabelled
					end example
			end data labelling;
		let ush feat label = float_of_int (count_pair feat label) /. float_of_int (count_feat feat) in
		let h feat label =
			let s = smoothing (count_pair feat label) (count_feat feat) (count_feat_unlabelled feat) in
				(float_of_int (count_pair feat label) +. s) /. (float_of_int (count_feat feat) +. (float_of_int (List.length labels)) *. s) in
			h, ush, count_feat

	let learn ?force ?(smoothing=fun _ _ _ -> 0.0) ?(threshold=0.0) ?(max_rules=max_int) labels features data labelling =
		let h, ush, count = counts labels features data labelling smoothing in
		let rules = List.concat & List.map begin fun label ->
				let over = List.filter (fun x -> ush x label > threshold) features in
					List.map (fun x -> (h x label, x, label)) & List.take_upto max_rules & List.stable_sort (fun x1 x2 -> count x2 - count x1) over
			end labels in
		let with_forced = match force with Some force -> force.rules @ rules | None -> rules in
			of_rules labels features with_forced
end
