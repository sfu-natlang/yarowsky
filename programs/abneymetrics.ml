(*
	Program to calculate the metrics of Abney 2004.
*)

open Utils
open Stats

let theta labels dl = 
	let uniform = 1. /. float_of_int (List.length labels) in
		fun feat ->
			normalize begin fun label ->
				try snd & Decision_list.score dl label feat
				with Not_found -> uniform
			end labels

let example_h labels dl example label use_sum =
	let example_len = List.length example in
	let num_labels = List.length labels in
	let uniform = 1. /. float_of_int num_labels in
	let theta_fs, phi, pi =
		let theta_fs = List.map (fun f -> theta labels dl f) example in
		let phi j = match label with Some label -> if j = label then 1. else 0. | None -> uniform in
		let pi =
			if use_sum then fun j -> sumf (fun theta_f -> theta_f j) theta_fs /. float_of_int example_len
			else normalize (fun j -> maxf (fun theta_f -> theta_f j) theta_fs) labels in
			if false then begin
				check_normalization "phi" phi labels;
				check_normalization "pi" pi labels;
				List.iter (fun theta_f -> check_normalization "theta_f" theta_f labels) theta_fs;
			end;
			theta_fs, phi, pi in
	let bound_ent, bound_div =
		let ent = ref 0. and div = ref 0. in
			List.iter begin fun theta_f ->
				ent := !ent +. entropy theta_f labels;
				List.iter begin fun theta_g ->
					div := !div +. kl_divergence theta_f theta_g labels
				end theta_fs
			end theta_fs;
			!ent, !div in
	let ent_phi = entropy phi labels in
	let div_phipi = kl_divergence phi pi labels in
	let ent_pi = entropy pi labels in
		[ent_phi +. div_phipi; ent_phi; ent_pi; div_phipi; bound_ent +. bound_div; bound_ent; bound_div]

let sum_lists lists =
	let len = List.length (List.hd lists) in
		List.fold_left begin fun totals list ->
			assert (List.length totals = len);
			assert (List.length list = len);
			List.map2 begin fun total x ->
				total +. x
			end totals list
		end (List.make len 0.) lists

let data_stats labels dl data labelling use_sum =
	sum_lists begin
		List.map2 begin fun example label ->
			example_h labels dl example label use_sum
		end data labelling
	end

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~min:6 ~max:6 Sys.argv;
			let lookup = Data.make_lookup () in
			let labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let dl = open_in_with (Decision_list.load lookup) (arg ()) in
			let data = open_in_with (Data.load_examples lookup) (arg ()) in
			let labelling = open_in_with (Data.load_labelling lookup) (arg ()) in
			let use_sum = bool_of_string (arg ()) in
			let results = data_stats labels dl data labelling use_sum in
				print_string (String.concat " " (List.map string_of_float results));
				print_newline ()
	with Arg_error ->
		Printf.eprintf "usage: %s LABELS DL DATA LABELLING USE-SUM\n" Sys.argv.(0);
		exit 1
