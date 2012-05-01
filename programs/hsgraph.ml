(*
	Program for the Yarowsky algorithm (Yarowsky 1995) as described in Collins & Singer 1999.
*)

open Utils

let majority labels dists =
	let uniform = 1.0 /. float_of_int (List.length labels) in
	let votes = setup (Data.Label.Table.make labels 0) begin fun votes ->
			List.iter begin fun dist ->
				let label, score = argmaxf snd (List.map (fun l -> (l, dist l)) labels) in
					if score > uniform then Data.Label.Table.add_val label 1 votes
			end dists
		end in
	let choices = List.map (fun l -> (l, Data.Label.Table.get l votes)) labels in
	let best, most_votes = argmaxi snd choices in
	let _, least_votes = argmini snd choices in
		if most_votes > least_votes then fun label -> if label = best then 1.0 else 0.0
		else fun label -> uniform
let average labels dists =
	let total = ref 0.0 in
	let votes = Data.Label.Table.make labels 0.0 in
		List.iter begin fun dist ->
			List.iter begin fun label ->
				let value = dist label in
					total := !total +. value;
					Data.Label.Table.addf_val label value votes
			end labels
		end dists;
		fun label ->
			Data.Label.Table.get label votes /. !total

let apply_phi labels =
	let uniform = 1.0 /. float_of_int (List.length labels) in
	fun phi ->
		let choices = List.map (fun l -> (l, phi l)) labels in
		let label, score = argmaxf snd choices in
			if score > uniform then Some label
			else None

let train ?(on_iter=Data.Saver.null) labels features data seed_dl max_iters feat_combine example_combine =
	let uniform = 1.0 /. float_of_int (List.length labels) in
	let is_uniform = Stats.is_uniform labels in
	let data_array = Array.of_list data in
	let feature_examples = setup (Data.Feature.Table.make features []) begin fun feature_examples ->
			List.iteri begin fun i example ->
				List.iter begin fun feat ->
					Data.Feature.Table.set feat (i :: Data.Feature.Table.get feat feature_examples) feature_examples
				end example
			end data
		end in
	let init_example_dists = setup (Array.map (fun _ -> fun _ -> uniform) data_array) begin fun init ->
			let seed_labelling = List.map (Decision_list.apply seed_dl) data in
				List.iteri begin fun i -> function
					| Some seed_label -> init.(i) <- fun l -> if l = seed_label then 1.0 else 0.0
					| None -> ()
				end seed_labelling
		end in
	let init_feat_dists = Data.Feature.Table.init features begin fun feat ->
			fun _ -> uniform
		end in
	let update_feat_dists example_dists feat_dists =
		Data.Feature.Table.init features begin fun feat ->
			let new_dist = feat_combine labels (List.map (fun i -> example_dists.(i)) (Data.Feature.Table.get feat feature_examples)) in
				if not (is_uniform new_dist) then new_dist
				else Data.Feature.Table.get feat feat_dists
		end in
	let make_example_dist feat_dists example = example_combine labels (List.rev (List.fold_left (fun ds f -> try Data.Feature.Table.get f feat_dists :: ds with Not_found -> ds) [] example)) in
	let update_example_dists feat_dists example_dists =
		Array.mapi begin fun i example ->
			let new_dist = make_example_dist feat_dists example in
				if not (is_uniform new_dist) then new_dist
				else example_dists.(i)
		end data_array in
	let rec run iter example_dists feat_dists old_labelling =
		let new_feat_dists = update_feat_dists example_dists feat_dists in
		let new_example_dists = update_example_dists new_feat_dists example_dists in
		let labelling = List.mapi (fun i _ -> apply_phi labels new_example_dists.(i)) data in
		let apply_dist = make_example_dist new_feat_dists in
		let _, coverage = Data.check_coverage labelling in
		let num_changes = Data.compare_labellings labelling old_labelling in
			assert begin
				List.iteri (fun i _ -> Stats.check_normalization "example" new_example_dists.(i) labels) data;
				List.iter (fun f -> Stats.check_normalization "feature" (Data.Feature.Table.get f new_feat_dists) labels) features;
				List.iteri (fun _ x -> Stats.check_normalization "apply" (apply_dist x) labels) data;
				true
			end;
			Printf.printf "iteration %i %i %f\n" iter num_changes coverage;
			flush stdout;
			let continue = iter < max_iters && num_changes > 0 in
				on_iter iter (not continue) (fun x -> apply_phi labels (apply_dist x));
				if continue then run (iter + 1) new_example_dists new_feat_dists labelling in
		run 1 init_example_dists init_feat_dists (List.map (fun _ -> None) data)

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:9 ~min:0 Sys.argv;
			let lookup = Data.make_lookup () in
			let labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let features = open_in_with (Data.load_features lookup) (arg ()) in
			let train_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let test_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let seed_dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let max_iters = int_of_string (arg ()) in
			let parse_combine = function
				| "majority" -> majority
				| "average" -> average
				| _ -> raise (Invalid_argument "combine") in
			let feat_combine = parse_combine (arg ()) in
			let example_combine = parse_combine (arg ()) in
				train ~on_iter:(Data.Saver.make lookup train_data test_data) labels features train_data seed_dl max_iters feat_combine example_combine
	with Arg_error ->
		Printf.eprintf "usage: %s LABELS FEATURES TRAIN-DATA TEST-DATA SEED-DL MAX-ITERS FEATURES-TO-EXAMPLE EXAMPLES-TO-FEATURES\n" Sys.argv.(0);
		exit 1
