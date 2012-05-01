(*
	Program to apply a saved decision list to data.
*)

open Utils

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:5 ~min:5 Sys.argv;
			let lookup = Data.make_lookup () in
			let dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let train_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let test_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let apply = match arg () with "max" -> Decision_list.apply | "sum" -> Decision_list.apply_sum | _ -> raise (Invalid_argument "combine") in
			let on_iter = Data.Saver.make ~save_classifier:(Decision_list.save lookup) lookup train_data test_data in
				on_iter 1 false ~classifier:dl (apply dl)
	with Arg_error ->
		Printf.eprintf "usage: %s DL TRAIN-DATA TEST-DATA COMBINE\n" Sys.argv.(0);
		exit 1
