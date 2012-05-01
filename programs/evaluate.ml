(*
	Program to evaluate one or more labellings against a gold standard.
*)

open Utils

let evaluate ?skip_labelling ?control_labelling known_labels gold_labelling eval_labelling =
	let size = List.length eval_labelling in
	let skipping =
		match skip_labelling with
		| Some labelling -> List.map (function Some _ -> true | None -> false) labelling
		| None -> List.make size false in
	let new_control_labelling =
		match control_labelling with
		| Some labelling -> labelling
		| None -> List.make size None in
	let labelled, correct, known_gold, skipped = ref 0, ref 0, ref 0, ref 0 in
	let match_table = Array.init 2 (fun i -> Array.Matrix.make 2 2 0) in
	let iob = function true -> 1 | false -> 0 in
	let incr counter = counter := !counter + 1 in
		assert (List.length gold_labelling = size);
		assert (List.length skipping = size);
		List.iter4 begin fun gold_label eval_label skip control_label ->
			if skip then incr skipped
			else begin
				let is_clean = match gold_label with Some l -> List.mem l known_labels | None -> false in
					if eval_label <> None then incr labelled;
					if eval_label = gold_label then incr correct;
					if is_clean then incr known_gold;
					option begin fun control_labelling ->
						let i = iob (control_label = gold_label) and j = iob (eval_label = gold_label) in
							match_table.(0).(i).(j) <- match_table.(0).(i).(j) + 1;
							if is_clean then match_table.(1).(i).(j) <- match_table.(1).(i).(j) + 1
					end control_labelling
			end
		end gold_labelling eval_labelling skipping new_control_labelling;
	let main_output =
		List.map begin fun (label, part, total) ->
			Printf.sprintf "%s: %i / %i = %f" label part total (float_of_int part /. float_of_int total)
		end [
			"skipped", !skipped, size;
			"coverage", !labelled, size - !skipped;
			"noise accuracy", !correct, size - !skipped;
			"clean accuracy", !correct, !known_gold;
		] in
	let control_output =
		match control_labelling with
		| None -> []
		| Some _ ->
			List.map begin fun (label, is_clean) ->
				let tab = match_table.(iob is_clean) in
					Printf.sprintf "%s: %i %i %i %i" label tab.(1).(1) tab.(1).(0) tab.(0).(1) tab.(0).(0)
			end [
				"noise compare", false;
				"clean compare", true;
			] in
		print_string (String.concat " " (List.concat [main_output; control_output]));
		print_newline ()

let _ =
	try
		let argv, opts = get_opts [] ["s"; "c"] Sys.argv in
		let arg, peek_arg, args_left = arg_popper argv in
			check_arg_num ~min:4 argv;
			let lookup = Data.make_lookup () in
			let load_labelling = open_in_with (fun ic -> Data.load_labelling lookup ic) in
			let known_labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let gold_labelling = load_labelling (arg ()) in
			let skip_labelling = try Some (load_labelling (List.assoc "s" opts)) with Not_found -> None in
			let control_labelling = try Some (load_labelling (List.assoc "c" opts)) with Not_found -> None in
			let eval_labelling_fns = args_left () in
				Array.iter begin fun fn ->
					evaluate known_labels gold_labelling ?skip_labelling:skip_labelling ?control_labelling:control_labelling (load_labelling fn);
					flush stdout
				end eval_labelling_fns
	with Arg_error ->
		Printf.eprintf "usage: %s [-s SKIP-LABELLING] [-c CONTROL-LABELLING] KNOWN-LABELS GOLD-LABELLING LABELLING [...]\n" Sys.argv.(0);
		exit 1
