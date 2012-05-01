(*
	Graph propagation of Subramanya et al. 2010.
*)

open Utils

let propagate table_init table_get nodes neighbours labels is_seeded seed_dist free_dist weight mu nu iters =
	let num_nodes = List.length nodes in
	let num_pairs = sumi (fun x -> List.length (neighbours x)) nodes in
	let seeded_nodes = List.filter is_seeded nodes in
	let uniform = 1.0 /. float_of_int (List.length labels) in
	let uniform_dist _ = uniform in
	let distance dist_x1 dist_x2 = sumf (fun y -> (dist_x1 y -. dist_x2 y)**2.) labels in
	let rec run i last_dist results =
		if i <= iters then begin
			let table = table_init begin fun x ->
					let kappa = (if is_seeded x then 1.0 else 0.0) +. nu +. mu *. sumf (weight x) (neighbours x) in
					Data.Label.Table.init labels begin fun y ->
						let gamma = (if is_seeded x then seed_dist x y else 0.) +. mu *. sumf (fun x1 -> weight x x1 *. last_dist x1 y) (neighbours x) +. nu *. uniform in
							gamma /. kappa
					end
				end in
			let dist x y = try Data.Label.Table.get y (table_get x table) with Not_found -> uniform in
			let c1 = sumf (fun x -> distance (seed_dist x) (dist x)) seeded_nodes in
			let c2 = mu *. sumf (fun x1 -> sumf (fun x2 -> weight x1 x2 *. distance (dist x1) (dist x2)) (neighbours x1)) nodes in
			let c3 = nu *. sumf (fun x -> distance (dist x) uniform_dist) nodes in
				Printf.printf "propagation iteration %i: %i %i %f + %f + %f = %f\n" i num_nodes num_pairs c1 c2 c3 (c1 +. c2 +. c3);
				run (i + 1) dist (dist::results)
		end else results in
	let results = run 1 free_dist [] in
		List.hd results, List.rev results
