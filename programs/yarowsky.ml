(*
	Program for the Yarowsky algorithm (Yarowsky 1995) as described in Collins & Singer 1999.
*)

open Utils

let train ?(on_iter=Data.Saver.null) labels features data seed_dl max_iters smoothing threshold cautious apply =
	let rec run iter cautious_num old_dl old_labelling =
		let labelling = List.map (apply old_dl) data in
		let num_changes = Data.compare_labellings labelling old_labelling in
		let _, coverage = Data.check_coverage labelling in
		let dl = Decision_list.Collins_singer.learn ~force:seed_dl ?smoothing:smoothing ~threshold:threshold ?max_rules:cautious_num labels features data labelling in
			Printf.printf "iteration %i %i %i %i %f\n" iter (match cautious_num with Some c -> c | None -> -1) (Decision_list.length dl) num_changes coverage;
			flush stdout;
			let continue = iter < max_iters && num_changes > 0 in
				on_iter iter (not continue) ~classifier:dl (apply dl);
				if continue then run (iter + 1) (match cautious_num, cautious with Some cn, Some c -> Some (cn + c) | _, _ -> None) dl labelling in
	run 1 cautious seed_dl (List.map (fun _ -> None) data)

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:11 ~min:11 Sys.argv;
			let lookup = Data.make_lookup () in
			let labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let features = open_in_with (Data.load_features lookup) (arg ()) in
			let train_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let test_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let seed_dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let max_iters = int_of_string (arg ()) in
			let smoothing = match arg () with "dl1" -> Some (Abney.dl_1_vs_smoothing labels) | s -> let n = float_of_string s in if n < 0.0 then None else Some (fun _ _ _ -> n) in
			let threshold = float_of_string (arg ()) in
			let cautious = let n = int_of_string (arg ()) in if n >= 0 then Some n else None in
			let combine = match arg () with "max" -> Decision_list.apply | "sum" -> Decision_list.apply_sum | _ -> raise (Invalid_argument "combine") in
				train ~on_iter:(Data.Saver.make ~save_classifier:(Decision_list.save lookup) lookup train_data test_data) labels features train_data seed_dl max_iters smoothing threshold cautious combine
	with Arg_error ->
		Printf.eprintf "usage: %s LABELS FEATURES TRAIN-DATA TEST-DATA SEED-DL MAX-ITERS SMOOTHING THRESHOLD CAUTIOUS COMBINE\n" Sys.argv.(0);
		exit 1
