(*
	Program for supervised decision list training.
*)

open Utils

let train ?(on_iter=Data.Saver.null) ?force_dl ?smoothing ?threshold ?max_rules labels features data labelling =
	let dl = Decision_list.Collins_singer.learn ?force:force_dl ?smoothing:smoothing ?threshold ?max_rules labels features data labelling in
		let new_labelling = List.map (Decision_list.apply dl) data in
		let num_changes = Data.compare_labellings labelling new_labelling in
		let _, coverage = Data.check_coverage labelling in
		Printf.printf "iteration %i %i %i %f\n" 1 (Decision_list.length dl) num_changes coverage;
		flush stdout;
		on_iter 1 true ~classifier:dl (Decision_list.apply dl)

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:10 ~min:10 Sys.argv;
			let lookup = Data.make_lookup () in
			let labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let features = open_in_with (Data.load_features lookup) (arg ()) in
			let train_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let test_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let labelling = open_in_with (Data.load_labelling lookup) (arg ()) in
			let force_dl = let f = arg () in if f = "-1" then None else Some (open_in_with (fun ic -> Decision_list.load lookup ic) f) in
			let smoothing = match arg () with "dl1" -> Some (Abney.dl_1_vs_smoothing labels) | s -> let n = float_of_string s in if n < 0.0 then None else Some (fun _ _ _ -> n) in
			let threshold = let n = float_of_string (arg ()) in if n < 0.0 then None else Some n in
			let max_rules = let n = int_of_string (arg ()) in if n < 0 then None else Some n in
				train ~on_iter:(Data.Saver.make ~save_classifier:(Decision_list.save lookup) lookup train_data test_data) ?force_dl:force_dl ?smoothing:smoothing ?threshold:threshold ?max_rules:max_rules labels features train_data labelling
	with Arg_error ->
		Printf.eprintf "usage: %s LABELS FEATURES TRAIN-DATA TEST-DATA LABELLING FORCE-DL SMOOTHING THRESHOLD MAX-RULES\n" Sys.argv.(0);
		exit 1
