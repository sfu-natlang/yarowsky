(*
	Things from Abney 2004.
*)

open Utils

let dl_1_vs_smoothing labels =
	let c = 1.0 /. float_of_int (List.length labels) in
		fun pair_count_labelled feat_count_labelled feat_count_unlabelled ->
			c *. float_of_int feat_count_unlabelled

let make_theta labels features dl =
	let uniform = 1.0 /. float_of_int (List.length labels) in
	let norms = Data.Feature.Table.init features begin fun feat ->
			sumf (fun y -> try snd (Decision_list.score dl y feat) with Not_found -> 0.0) labels
		end in
		fun feat label ->
			try
				let norm = Data.Feature.Table.get feat norms in
					if norm = 0.0 then uniform
					else (try snd (Decision_list.score dl label feat) with Not_found -> 0.0) /. norm
			with Not_found -> uniform
let make_pi labels theta combine example =
	let total = sumf (fun y -> combine (fun x -> theta x y) example) labels in
		fun label ->
			combine (fun x -> theta x label) example /. total

let apply_theta labels theta combine =
	let uniform = 1.0 /. float_of_int (List.length labels) in
	let no_uniform_theta x label =
		let v = theta x label in
			if v <> uniform then v
			else 0.0 in
		fun _ example ->
			let score label = combine (fun x -> no_uniform_theta x label) example in
			let choices = List.map (fun y -> (y, score y)) labels in
			let sorted = List.stable_sort (fun (_, s1) (_, s2) -> sign (s2 -. s1)) choices in
				if List.length sorted > 0 then begin
					let best, s = List.hd sorted in
						if s <> 0.0 then Some best
						else None
				end else None
