(*
	Program to apply a saved decision list to data.
*)

open Utils

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:3 ~min:3 Sys.argv;
			let lookup = Data.make_lookup () in
			let dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let data = open_in_with (Data.load_examples lookup) (arg ()) in
			let labelling = List.map (Decision_list.apply dl) data in
				Data.save_labelling lookup labelling stdout
	with Arg_error ->
		Printf.eprintf "usage: %s DL DATA\n" Sys.argv.(0);
		exit 1
