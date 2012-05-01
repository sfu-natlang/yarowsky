(*
	Data types and IO for common data.
*)
	
open Utils

module Label =
struct
	type label = Label of int
	type t = label

	exception Unknown of string

	module Map = Map.Make(struct type t = label;; let compare = compare end)
	module Set = Set.Make(struct type t = label;; let compare = compare end)

	module Table =
	struct
		type 'a t = {
			lookup : 'a array;
			max_label_id : int;
		}

		let init labels f =
			let max_label_id = try maxi (fun (Label li) -> li) labels with Not_found -> 0 in
				{
					lookup = Array.init (List.length labels) (fun li -> f (Label li));
					max_label_id = max_label_id;
				}
		let make labels value =
			init labels (fun _ -> value)
		let setup f table =
			for li = 0 to table.max_label_id do
				table.lookup.(li) <- f (Label li) table.lookup.(li)
			done
		let copy table =
			{
				lookup = Array.copy table.lookup;
				max_label_id = table.max_label_id;
			}
		let map f table =
			{
				lookup = Array.map f table.lookup;
				max_label_id = table.max_label_id;
			}
		let iteri f table =
			Array.iteri begin fun li x ->
				f (Label li) x
			end table.lookup
		let mem (Label li) table =
			li <= table.max_label_id
		let get (Label li) table =
			if li <= table.max_label_id then table.lookup.(li)
			else raise Not_found
		let set (Label li) value table =
			table.lookup.(li) <- value
		let add_val (Label li) change table =
			table.lookup.(li) <- table.lookup.(li) + change
		let addf_val (Label li) change table =
			table.lookup.(li) <- table.lookup.(li) +. change
	end
end

module Feature =
struct
	type feature = Feature of int
	type t = feature

	exception Unknown of string

	module Map = Map.Make(struct type t = feature;; let compare = compare end)
	module Set = Set.Make(struct type t = feature;; let compare = compare end)

	module Table =
	struct
		type 'a t = {
			lookup : 'a array;
			max_feat_id : int;
		}

		let init features f =
			let max_feat_id = try maxi (fun (Feature fi) -> fi) features with Not_found -> 0 in
				{
					lookup = Array.init (max_feat_id + 1) (fun fi -> f (Feature fi));
					max_feat_id = max_feat_id;
				}
		let make features value =
			init features (fun _ -> value)
		let setup f table =
			for fi = 0 to table.max_feat_id do
				table.lookup.(fi) <- f (Feature fi) table.lookup.(fi)
			done
		let copy table =
			{
				lookup = Array.copy table.lookup;
				max_feat_id = table.max_feat_id;
			}
		let map f table =
			{
				lookup = Array.map f table.lookup;
				max_feat_id = table.max_feat_id;
			}
		let iteri f table =
			Array.iteri begin fun fi x ->
				f (Feature fi) x
			end table.lookup
		let mem (Feature fi) table =
			fi <= table.max_feat_id
		let get (Feature fi) table =
			if fi <= table.max_feat_id then table.lookup.(fi)
			else raise Not_found
		let set (Feature fi) value table =
			table.lookup.(fi) <- value
		let add_val (Feature fi) change table =
			table.lookup.(fi) <- table.lookup.(fi) + change
		let addf_val (Feature fi) change table =
			table.lookup.(fi) <- table.lookup.(fi) +. change
	end
end

module StrMap = Map.Make(struct type t = string;; let compare = compare end)

type lookup = {
		mutable next_label_id : int;
		mutable next_feat_id : int;
		mutable str_to_label : Label.t StrMap.t;
		mutable label_to_str : string Label.Map.t;
		mutable str_to_feat : Feature.t StrMap.t;
		mutable feat_to_str : string Feature.Map.t;
	}

let make_lookup () =
	{
		next_label_id = 0;
		next_feat_id = 0;
		str_to_feat = StrMap.empty;
		feat_to_str = Feature.Map.empty;
		str_to_label = StrMap.empty;
		label_to_str = Label.Map.empty;
	}
let string_of_label lookup label =
	Label.Map.find label lookup.label_to_str
let string_of_feature lookup feat =
	Feature.Map.find feat lookup.feat_to_str
let make_label lookup str =
	try
		StrMap.find str lookup.str_to_label
	with Not_found ->
		let label = Label.Label lookup.next_label_id in
			lookup.str_to_label <- StrMap.add str label lookup.str_to_label;
			lookup.label_to_str <- Label.Map.add label str lookup.label_to_str;
			lookup.next_label_id <- lookup.next_label_id + 1;
			label
let make_feature lookup str =
	try
		StrMap.find str lookup.str_to_feat
	with Not_found ->
		let feat = Feature.Feature lookup.next_feat_id in
			lookup.str_to_feat <- StrMap.add str feat lookup.str_to_feat;
			lookup.feat_to_str <- Feature.Map.add feat str lookup.feat_to_str;
			lookup.next_feat_id <- lookup.next_feat_id + 1;
			feat
let load_labels lookup = lines_to_list (make_label lookup)
let load_features lookup = lines_to_list (make_feature lookup)

let feature_set features =
	List.fold_left (flip Feature.Set.add) Feature.Set.empty features
let uniq_labels list =
	let used = ref Label.Set.empty in
		List.filter (fun l -> if not (Label.Set.mem l !used) then (used := Label.Set.add l !used; true) else false) list
let uniq_features list =
	let used = ref Feature.Set.empty in
		List.filter (fun f -> if not (Feature.Set.mem f !used) then (used := Feature.Set.add f !used; true) else false) list

let load_examples lookup =
	let space = Str.regexp "[ \t]+" in
		lines_to_list begin fun line ->
			uniq_features (List.map (fun feat_str -> make_feature lookup feat_str) (Str.split space line))
		end
let load_labelling lookup =
	lines_to_list begin fun label_str ->
		if String.length label_str > 0 then Some (make_label lookup label_str) else None
	end
let save_labelling lookup labelling oc =
	List.iter begin fun label_opt ->
		begin match label_opt with
		| Some label -> output_string oc (Label.Map.find label lookup.label_to_str)
		| None -> ()
		end;
		output_char oc '\n'
	end labelling

module Table =
struct
	type 'a t = {
		matrix : 'a array array;
		max_label_id : int;
		max_feat_id : int;
	}

	let init labels features f =
		let max_feat_id = try maxi (fun (Feature.Feature fi) -> fi) features with Not_found -> 0 in
		let max_label_id = try maxi (fun (Label.Label li) -> li) labels with Not_found -> 0 in
			{
				matrix = Array.Matrix.init (List.length labels) (max_feat_id + 1) (fun li fi -> f (Label.Label li) (Feature.Feature fi));
				max_label_id = max_label_id;
				max_feat_id = max_feat_id;
			}
	let make labels features value =
		init labels features (fun _ _ -> value)
	let setup f table =
		for li = 0 to table.max_label_id do
			for fi = 0 to table.max_feat_id do
				table.matrix.(li).(fi) <- f (Label.Label li) (Feature.Feature fi) table.matrix.(li).(fi)
			done
		done
	let copy table =
		{
			matrix = Array.Matrix.copy table.matrix;
			max_label_id = table.max_label_id;
			max_feat_id = table.max_feat_id;
		}
	let map f table =
		{
			matrix = Array.Matrix.map f table.matrix;
			max_label_id = table.max_label_id;
			max_feat_id = table.max_feat_id;
		}
	let iteri f table =
		Array.Matrix.iteri begin fun li fi x ->
			f (Label.Label li) (Feature.Feature fi) x
		end table.matrix
	let mem (Label.Label li) (Feature.Feature fi) table =
		li <= table.max_label_id && fi <= table.max_feat_id
	let get (Label.Label li) (Feature.Feature fi) table =
		if li <= table.max_label_id && fi <= table.max_feat_id then table.matrix.(li).(fi)
		else raise Not_found
	let set (Label.Label li) (Feature.Feature fi) value table =
		table.matrix.(li).(fi) <- value
	let update_val (Label.Label li) (Feature.Feature fi) f table =
		table.matrix.(li).(fi) <- f table.matrix.(li).(fi)
	let add_val (Label.Label li) (Feature.Feature fi) change table =
		table.matrix.(li).(fi) <- table.matrix.(li).(fi) + change
	let addf_val (Label.Label li) (Feature.Feature fi) change table =
		table.matrix.(li).(fi) <- table.matrix.(li).(fi) +. change
end

let compare_labellings new_labelling old_labelling =
	let num_changes = ref 0 in
		List.iter2 begin fun new_label old_label ->
			if new_label <> old_label then num_changes := !num_changes + 1
		end new_labelling old_labelling;
		!num_changes
let check_coverage labelling =
	let num_labelled = ref 0 in
		List.iter begin fun label ->
			option begin fun _ ->
				num_labelled := !num_labelled + 1
			end label
		end labelling;
		!num_labelled, (float_of_int !num_labelled /. float_of_int (List.length labelling))

module Saver =
struct
	let null ?classifier ?extra_apply _ _ _ = ()
	let makei ?(save_iters=1) ?save_classifier lookup train_data test_data ?classifier ?extra_apply iter last main_apply =
			if iter mod save_iters == 0 || last then begin
				List.iter begin fun (suffix, apply) ->
					let label_data = List.mapi (fun i e -> apply i e) in
						open_out_with (save_labelling lookup & label_data test_data) (Printf.sprintf "%i%s.test" iter suffix);
						open_out_with (save_labelling lookup & label_data train_data) (Printf.sprintf "%i%s.train" iter suffix);
						begin match save_classifier, classifier with
						| Some save_classifier, Some classifier -> open_out_with (save_classifier classifier) (Printf.sprintf "%i.dl" iter)
						| _, _ -> ()
						end
				end (List.concat [[".main", main_apply]; match extra_apply with None -> [] | Some ea -> List.map (fun (n, a) -> assert (n <> "main"); Printf.sprintf ".%s" n, a) ea])
			end
	let make ?(save_iters=1) ?save_classifier lookup train_data test_data ?classifier ?extra_apply =
		let maker = makei ~save_iters:save_iters ?save_classifier lookup train_data test_data ?classifier in
		let extra_apply = match extra_apply with None -> None | Some ea -> Some (List.map (fun (n, a) -> (n, fun _ -> a)) ea) in
			fun iter last main_apply ->
				maker ?extra_apply:extra_apply iter last (fun _ -> main_apply)
end
