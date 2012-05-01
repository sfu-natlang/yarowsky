(*
	Program to compare correctness of multiple labellings on individual examples.
*)

open Utils

let compare gold_labelling other_labellings =
	let ol_arrays = Array.map Array.of_list other_labellings in
		List.iteri begin fun i gold_label ->
			let correctness = Array.map (fun a -> a.(i) = gold_label) ol_arrays in
				Array.iteri begin fun i correct ->
					Printf.printf "%s%b" (if i > 0 then " " else "") correct
				end correctness;
				Printf.printf "\n"
		end gold_labelling

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~min:3 Sys.argv;
			let lookup = Data.make_lookup () in
			let load_labelling = open_in_with (fun ic -> Data.load_labelling lookup ic) in
			let gold_labelling = load_labelling (arg ()) in
			let num_args_before_labellings = 2 in
			let other_labellings = Array.map load_labelling (Array.sub Sys.argv num_args_before_labellings (Array.length Sys.argv - num_args_before_labellings)) in
				compare gold_labelling other_labellings
	with Arg_error ->
		Printf.eprintf "usage: %s GOLD-LABELLING LABELLING ...\n" Sys.argv.(0);
		exit 1
