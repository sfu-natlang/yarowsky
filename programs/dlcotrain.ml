(*
	Program for DL-CoTrain of Collins & Singer 1999.
*)

open Utils

let split_data views data =
	Array.map begin fun view ->
		List.map (List.filter (flip Data.Feature.Set.mem view)) data
	end views

let split_dl views dl =
	Array.map begin fun view ->
		Decision_list.filter (fun (_, x, _) -> Data.Feature.Set.mem x view) dl
	end views

let train ?(on_iter=Data.Saver.null) labels features views data seed_dl max_iters smoothing threshold cautious =
	assert (List.length views = 2);
	let views = Array.of_list & List.map Data.feature_set views in
	let view_data = split_data views data in
	let view_seeds = split_dl views seed_dl in
	let dls = Array.copy view_seeds in
	let labellings = Array.map (fun _ -> List.map (fun _ -> None) data) views in
	let rec run iter cautious_num =
		let total_changes = ref 0 in
			List.iter begin fun (i, j) ->
				let labelling = List.map (Decision_list.apply dls.(i)) view_data.(i)  in
				let num_changes = Data.compare_labellings labelling labellings.(i) in
				let _, coverage = Data.check_coverage labelling in
				let dl = Decision_list.Collins_singer.learn ~force:view_seeds.(j) ?smoothing:smoothing ~threshold:threshold ~max_rules:cautious_num labels features view_data.(j) labelling in
					labellings.(i) <- labelling;
					dls.(j) <- dl;
					total_changes := !total_changes + num_changes;
					Printf.printf "iteration %i %i %i %i %i %f\n" iter i cautious_num (Decision_list.length dl) num_changes coverage;
					flush stdout;
			end [0, 1; 1, 0];
			let continue = iter < max_iters && !total_changes > 0 in
			let joint_dl = Decision_list.merge labels features (Array.to_list dls) in
				on_iter iter (not continue) ~classifier:joint_dl (Decision_list.apply joint_dl);
				if continue then run (iter + 1) (cautious_num + cautious) in
			run 1 cautious

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:12 ~min:12 Sys.argv;
			let lookup = Data.make_lookup () in
			let labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let features = open_in_with (Data.load_features lookup) (arg ()) in
			let view1 = open_in_with (Data.load_features lookup) (arg ()) in
			let view2 = open_in_with (Data.load_features lookup) (arg ()) in
			let train_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let test_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let seed_dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let max_iters = int_of_string (arg ()) in
			let smoothing = match arg () with "dl1" -> Some (Abney.dl_1_vs_smoothing labels) | s -> let n = float_of_string s in if n < 0.0 then None else Some (fun _ _ _ -> n) in
			let threshold = float_of_string (arg ()) in
			let cautious = int_of_string (arg ()) in
				train ~on_iter:(Data.Saver.make ~save_classifier:(Decision_list.save lookup) lookup train_data test_data) labels features [view1; view2] train_data seed_dl max_iters smoothing threshold cautious
	with Arg_error ->
		Printf.eprintf "usage: %s LABELS FEATURES VIEW1 VIEW2 TRAIN-DATA TEST-DATA SEED-DL MAX-ITERS SMOOTHING THRESHOLD CAUTIOUS\n" Sys.argv.(0);
		exit 1
