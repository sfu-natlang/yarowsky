(*
	Utilities for statistics.
*)

open Utils

let entropy ?(base=2.) p domain =
	-. (sumf (fun x -> let px = p x in px *. if px = 0. then 0. else (log px /. log base)) domain)
let kl_divergence ?(base=2.) p q domain =
	sumf (fun x -> let px = p x in px *. if px = 0. then 0. else (log (px /. q x) /. log base)) domain

let normalize f domain =
	let total = sumf id (List.map f domain) in
		if total > 0. then fun x -> f x /. total
		else let size = List.length domain in fun _ -> 1. /. float_of_int size

let check_normalization name f domain =
	let values = List.map f domain in
	let total = sumf id values in
		if abs_float (total -. 1.) > 0.00001 then begin
			Printf.eprintf "normalization failure for %s: %f :" name total;
			List.iter (fun v -> Printf.eprintf " %f" v) values;
			Printf.eprintf "\n"
		end

let is_uniform domain =
	let uniform = 1.0 /. float_of_int (List.length domain) in
		fun dist -> maxf dist domain = uniform
