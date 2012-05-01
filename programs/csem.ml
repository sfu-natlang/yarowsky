(*
	Program for the EM algorithm of described in Collins & Singer 1999.
*)

open Utils

let save_model lookup labels features lengths (p_y, p_m, p_xy) os =
	List.iter begin fun label ->
		let p = p_y label in
			if p <> 0. then
				Printf.fprintf os "p_y\t%s\t%f\n" (Data.string_of_label lookup label) p
	end labels;
	List.iter begin fun length ->
		let p = p_m length in
			if p <> 0. then
				Printf.fprintf os "p_m\t%i\t%f\n" length p
	end lengths;
	List.iter begin fun feat ->
		List.iter begin fun label ->
			let p = p_xy feat label in
				if p <> 0. then
					Printf.fprintf os "p_xy\t%s\t%s\t%f\n" (Data.string_of_label lookup label) (Data.string_of_feature lookup feat) p
		end labels
	end features

let apply_pi labels pi example =
	Some (argmaxf (fun y -> pi example y) labels)

let make_getters lookup_y lookup_m lookup_xy max_m def_y def_m def_xy =
	let get_y y = try Data.Label.Table.get y lookup_y with Not_found -> def_y in
	let get_m m = if m <= max_m then lookup_m.(m) else def_m in
	let get_xy x y = try Data.Table.get y x lookup_xy with Not_found -> def_xy y in
		(get_y, get_m, get_xy)

let make_counts ?(hard=false) labels features lengths data seed_labelling fixed_weight hidden_weight pi =
	let max_m = maxi id lengths in
	let count_y = Data.Label.Table.make labels 0. in
	let count_m = Array.make (max_m + 1) 0. in
	let count_xy = Data.Table.make labels features 0. in
	let add label example weight =
		let m = List.length example in
			Data.Label.Table.addf_val label weight count_y;
			count_m.(m) <- count_m.(m) +. weight;
			List.iter (fun feat -> Data.Table.addf_val label feat weight count_xy) example in
		List.iter2 begin fun example seed_label ->
				match seed_label with
				| Some seed_label ->
					add seed_label example fixed_weight
				| None ->
					let choices = List.map (fun y -> pi example y, y) labels in
						if hard then begin
							let p, label = argmaxf fst choices in
								add label example hidden_weight
						end else begin
							let norm = sumf fst choices in
								List.iter begin fun (p, label) ->
									add label example (p *. hidden_weight /. norm)
								end choices
						end
				end data seed_labelling;
		make_getters count_y count_m count_xy max_m 0. 0. (fun _ -> 0.)

let make_probs ?(smoothing=0.0) labels features lengths (g_y, g_m, g_xy) =
	let max_m = maxi id lengths in
	let prob_y = Data.Label.Table.make labels 0. in
	let prob_m = Array.make (max_m + 1) 0. in
	let prob_xy = Data.Table.make labels features 0. in
	let norm_y = sumf (fun y -> g_y y +. smoothing) labels in
	let norm_m = sumf (fun m -> g_m m +. smoothing) lengths in
	let norm_xy = Data.Label.Table.make labels 0. in
		List.iter begin fun label ->
			let norm = sumf (fun x -> g_xy x label +. smoothing) features in
				Data.Label.Table.set label norm norm_xy;
				Data.Label.Table.set label ((g_y label +. smoothing) /. norm_y) prob_y;
				List.iter begin fun feat ->
					Data.Table.set label feat ((g_xy feat label +. smoothing) /. norm) prob_xy
				end features
		end labels;
		List.iter begin fun m ->
			prob_m.(m) <- (g_m m +. smoothing) /. norm_m
		end lengths;
	let uniform_y = 1.0 /. float_of_int (List.length labels) in
		make_getters prob_y prob_m prob_xy max_m (smoothing /. norm_y) (smoothing /. norm_m) (fun y -> try smoothing /. Data.Label.Table.get y norm_xy with Not_found -> uniform_y)

let random_params rng labels features lengths =
	let rand () = 1. +. Random.State.float rng 1. in
	let max_m = maxi id lengths in
	let norm_y = ref 0. and norm_m = ref 0. and norm_xy = Data.Label.Table.make labels 0. in
	let prob_y = Data.Label.Table.init labels (fun _ -> let p = rand () in norm_y := !norm_y +. p; p) in
	let prob_m = Array.init (max_m + 1) (fun _ -> let p = rand () in norm_m := !norm_m +. p; p) in
	let prob_xy = Data.Table.init labels features (fun y _ -> let p = rand () in Data.Label.Table.addf_val y p norm_xy; p) in
	let uniform_y = 1.0 /. float_of_int (List.length labels) in
		Data.Label.Table.setup (fun y v -> v /. !norm_y) prob_y;
		for i = 0 to max_m do prob_m.(i) <- prob_m.(i) /. !norm_y done;
		Data.Table.setup (fun y x v -> v /. Data.Label.Table.get y norm_xy) prob_xy;
		make_getters prob_y prob_m prob_xy max_m 0. 0. (fun y -> if Data.Label.Table.mem y prob_y then 0. else uniform_y)

let make_pi (p_y, p_m, p_xy) example label =
	p_y label *. p_m (List.length example) *. prodf (fun x -> p_xy x label) example

let log_likelihood labels data seed_labelling pi =
	let ll = ref 0. in
		List.iter2 begin fun example seed_label ->
			let p = match seed_label with
				| Some seed_label -> pi example seed_label
				| None -> sumf (pi example) labels in
				ll := !ll +. log p
		end data seed_labelling;
		!ll

let check_normalization labels features lengths (p_y, p_m, p_xy) =
	Stats.check_normalization "p_y" p_y labels;
	Stats.check_normalization "p_m" p_m lengths;
	List.iter (fun y -> Stats.check_normalization "p_xy" (fun x -> p_xy x y) features) labels

let train ?(on_iter=Data.Saver.null) labels features lengths data seed_dl max_iters smoothing fixed_weight hidden_weight hard rng =
	let convergence_epsilon = 10.**(-.4.) in
	let seed_labelling = List.map (Decision_list.apply seed_dl) data in
	let rec run iter old_ll old_pi =
		let counts = make_counts ~hard:hard labels features lengths data seed_labelling fixed_weight hidden_weight old_pi in
		let probs = make_probs ~smoothing:smoothing labels features lengths counts in
		let pi = make_pi probs in
		let ll = log_likelihood labels data seed_labelling pi in
		let d_ll = ll -. old_ll in
			assert (check_normalization labels features lengths probs; true);
			Printf.printf "iteration %i %f %f\n" iter ll d_ll;
			flush stdout;
			if ll < old_ll then Printf.eprintf "warning: log-likelihood is decreasing by %f: %f to %f\n" (-. d_ll) old_ll ll;
			let continue = iter < max_iters && abs_float (ll -. old_ll) >= convergence_epsilon in
				on_iter iter (*~classifier:probs*) (not continue) (apply_pi labels pi);
				if continue then run (iter + 1) ll pi in
	let init_probs = random_params rng labels features lengths in
	let init_pi = make_pi init_probs in
	let init_ll = log_likelihood labels data seed_labelling init_pi in
		assert (check_normalization labels features lengths init_probs; true);
		Printf.printf "initial log likelihood %f\n" init_ll;
		run 1 init_ll init_pi

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:12 ~min:12 Sys.argv;
			let lookup = Data.make_lookup () in
			let labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let features = open_in_with (Data.load_features lookup) (arg ()) in
			let train_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let test_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let seed_dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let max_iters = int_of_string (arg ()) in
			let smoothing = float_of_string (arg ()) in
			let fixed_weight = float_of_string (arg ()) in
			let hidden_weight = float_of_string (arg ()) in
			let hard = bool_of_string (arg ()) in
			let rng_seed = int_of_string (arg ()) in
			let rng = Random.State.make [|rng_seed|] in
			let lengths = List.uniq (List.concat (List.map (List.map List.length) [train_data; test_data])) in
				train ~on_iter:(Data.Saver.make ~save_classifier:(save_model lookup labels features lengths) lookup train_data test_data) labels features lengths train_data seed_dl max_iters smoothing fixed_weight hidden_weight hard rng
	with Arg_error ->
		Printf.eprintf "usage: %s LABELS FEATURES TRAIN-DATA TEST-DATA SEED-DL MAX-ITERS SMOOTHING FIXED-WEIGHT HIDDEN-WEIGHT HARD RANDOM-SEED\n" Sys.argv.(0);
		exit 1
