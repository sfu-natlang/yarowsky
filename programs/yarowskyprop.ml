(*
	Program for a variation of the Yarowsky algorithm (Yarowsky 1995) as described in Collins & Singer 1999 that uses the graph propagation of Subramanya et al. 2010 on the bipartite graph of Haffari & Sarkar 2007.
*)

open Utils

let label_where_ok ok_labelling labelling =
	List.map2 begin fun ok_label label ->
		match ok_label with
		| Some _ -> label
		| None -> None
	end ok_labelling labelling

module BipartiteGraph =
struct
	type node = FeatureNode of Data.Feature.t | ExampleNode of int

	module NodeTable =
	struct
		type 'a t = {
			features : 'a Data.Feature.Table.t;
			examples : 'a array;
		}

		let make features examples value =
			{
				features = Data.Feature.Table.make features value;
				examples = Array.init (List.length examples) (fun _ -> value);
			}
		let init features examples f =
			{
				features = Data.Feature.Table.init features (fun g -> f (FeatureNode g));
				examples = Array.init (List.length examples) (fun i -> f (ExampleNode i));
			}
		let get node table =
			match node with
			| FeatureNode feat -> Data.Feature.Table.get feat table.features
			| ExampleNode index -> table.examples.(index)
		let set node x table =
			match node with
			| FeatureNode feat -> Data.Feature.Table.set feat x table.features
			| ExampleNode index -> table.examples.(index) <- x
	end

	let make_phi labels data labelling =
		let norm = 1.0 /. float_of_int (List.length labels) in
		let array = Array.of_list labelling in
			fun i example label ->
				match array.(i) with
				| Some y when y = label -> 1.0 
				| Some y -> 0.0
				| None -> norm

	let dist data feature_dist example_dist =
		let data_array = Array.of_list data in
			function
			| FeatureNode feat -> feature_dist feat
			| ExampleNode index -> example_dist index data_array.(index)
	let feature_dist node_dist f = node_dist (FeatureNode f)
	let neighbours features data =
		let example_lookup = Array.map (fun x -> List.map (fun f -> FeatureNode f) x) (Array.of_list data) in
		let feat_lookup = Data.Feature.Table.make features [] in
			List.iteri begin fun index example ->
				List.iter begin fun feat ->
					try Data.Feature.Table.set feat (ExampleNode index :: Data.Feature.Table.get feat feat_lookup) feat_lookup
					with Not_found -> ()
				end example
			end data;
			function
			| FeatureNode feat -> Data.Feature.Table.get feat feat_lookup
			| ExampleNode index -> example_lookup.(index)
	let nodes features examples =
		List.concat [List.map (fun f -> FeatureNode f) features; List.mapi (fun i _ -> ExampleNode i) examples]

	type prop_type = UsePi | UsePhi

	let propagate prop_type labels features data seed_dl combine prop_mu prop_nu prop_iters =
		let neighbours = neighbours features data in
		let propagate = Subramanya.propagate (NodeTable.init features data) NodeTable.get (nodes features data) neighbours labels (fun _ -> false) in
			fun theta pi ->
				let example_dist =
					match prop_type with
					| UsePi -> (fun i x -> pi x)
					| UsePhi ->
						let phi = make_phi labels data (List.mapi (Abney.apply_theta labels theta combine) data) in
							assert (List.iteri (fun i x -> Stats.check_normalization "phi" (phi i x) labels) data; true);
							phi in
				let propagated, steps = propagate (fun _ -> assert false) (dist data theta example_dist) (fun _ _ -> 1.0) prop_mu prop_nu prop_iters in
					feature_dist propagated, List.map feature_dist steps
end

module ThetaOnlyGraph =
struct
	type prop_type = UseTheta | UseThetaType

	let make_theta_type labels features data pi =
		let values = Data.Table.make labels features 0.0 in
		let norms = Data.Feature.Table.make features 0 in
			List.iter begin fun example ->
				List.iter begin fun label ->
					let p = pi example label in
						List.iter begin fun feat ->
							Data.Table.addf_val label feat p values
						end example
				end labels;
				List.iter begin fun feat ->
					Data.Feature.Table.add_val feat 1 norms;
				end example
			end data;
			fun feat label -> Data.Table.get label feat values /. float_of_int (Data.Feature.Table.get feat norms)

	let propagate prop_type labels features data seed_dl combine prop_mu prop_nu prop_iters =
		let neighbours =
			let lookup = Data.Feature.Table.make features Data.Feature.Set.empty in
				List.iter begin fun example ->
					List.iter begin fun feat1 ->
						List.iter begin fun feat2 ->
							if feat1 <> feat2 then
								Data.Feature.Table.set feat1 (Data.Feature.Set.add feat2 (Data.Feature.Table.get feat1 lookup)) lookup
						end example
					end example
				end data;
				let list_lookup = Data.Feature.Table.map (fun s -> Data.Feature.Set.elements s) lookup in
					flip Data.Feature.Table.get list_lookup in
		let propagate = Subramanya.propagate (Data.Feature.Table.init features) Data.Feature.Table.get features neighbours labels (fun _ -> false) in
			fun theta pi ->
				let dist =
					match prop_type with
					| UseTheta -> theta
					| UseThetaType ->
						let theta_type = make_theta_type labels features data pi in
							List.iter (fun x -> Stats.check_normalization "theta type" (theta_type x) labels) features;
							theta_type in
					propagate (fun _ -> assert false) dist (fun _ _ -> 1.0) prop_mu prop_nu prop_iters
end

let train ?(on_iter=Data.Saver.null) labels features data seed_dl init_dl max_iters smoothing threshold cautious combine propagater prop_mu prop_nu prop_iters =
	let propagate = propagater labels features data seed_dl combine prop_mu prop_nu prop_iters in
	let rec run iter cautious_num old_dl old_labelling continue =
		let theta = Abney.make_theta labels features old_dl in
		let pi = Abney.make_pi labels theta combine in
		let propagated, steps = propagate theta pi in
		let labelling = label_where_ok (List.mapi (Abney.apply_theta labels theta combine) data) (List.mapi (Abney.apply_theta labels propagated combine) data) in
		let dl = Decision_list.Collins_singer.learn ~force:seed_dl ?smoothing:smoothing ~threshold:threshold ?max_rules:cautious_num labels features data labelling in
		let num_changes = Data.compare_labellings labelling old_labelling in
		let continue_now = iter < max_iters && num_changes > 0 in
			assert begin
				List.iter (fun x -> Stats.check_normalization "theta" (theta x) labels) features;
				List.iter (fun e -> Stats.check_normalization "pi" (pi e) labels) data;
				List.iter (fun x -> Stats.check_normalization "propagated" (propagated x) labels) features;
				true
			end;
			begin
				let _, coverage = Data.check_coverage labelling in
				let step_applies = List.mapi (fun i t -> (Printf.sprintf "prop%i" i, Abney.apply_theta labels t combine)) steps in
				let dl_apply = let a = Decision_list.apply dl in fun _ -> a in
				let theta_apply = Abney.apply_theta labels theta combine in
				let prop_apply = Abney.apply_theta labels propagated combine in
					Printf.printf "iteration %i %i %i %i %f\n" iter (match cautious_num with Some c -> c | None -> -1) (Decision_list.length dl) num_changes coverage;
					flush stdout;
					on_iter iter (not continue_now || not continue) ~classifier:dl ~extra_apply:(List.concat [["dl", dl_apply; "theta", theta_apply]; step_applies]) prop_apply;
			end;
			if continue then run (iter + 1) (match cautious_num, cautious with Some cn, Some c -> Some (cn + c) | _, _ -> None) dl labelling continue_now in
		run 1 cautious init_dl (List.map (fun _ -> None) data) true

let _ =
	try
		let arg, _, _ = arg_popper Sys.argv in
			check_arg_num ~max:17 ~min:17 Sys.argv;
			let lookup = Data.make_lookup () in
			let labels = open_in_with (Data.load_labels lookup) (arg ()) in
			let features = open_in_with (Data.load_features lookup) (arg ()) in
			let train_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let test_data = open_in_with (Data.load_examples lookup) (arg ()) in
			let seed_dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let init_dl = open_in_with (fun ic -> Decision_list.load lookup ic) (arg ()) in
			let max_iters = int_of_string (arg ()) in
			let smoothing = match arg () with "dl1" -> Some (Abney.dl_1_vs_smoothing labels) | s -> let n = float_of_string s in if n < 0.0 then None else Some (fun _ _ _ -> n) in
			let threshold = float_of_string (arg ()) in
			let cautious = let n = int_of_string (arg ()) in if n >= 0 then Some n else None in
			let combine = match arg () with "max" -> maxf | "sum" -> sumf | _ -> raise (Invalid_argument "combine") in
			let propagater =
				match arg () with
				| "thetaonly" -> ThetaOnlyGraph.propagate (match arg () with "theta" -> ThetaOnlyGraph.UseTheta | "thetatype" -> ThetaOnlyGraph.UseThetaType | _ -> raise (Invalid_argument "propagation subtype"))
				| "bipartite" -> BipartiteGraph.propagate (match arg () with "pi" -> BipartiteGraph.UsePi | "phi" -> BipartiteGraph.UsePhi | _ -> raise (Invalid_argument "propagation subtype"))
				| _ -> raise (Invalid_argument "propagation type") in
			let prop_mu = float_of_string (arg ()) in
			let prop_nu = float_of_string (arg ()) in
			let prop_iters = int_of_string (arg ()) in
				train ~on_iter:(Data.Saver.makei ~save_classifier:(Decision_list.save lookup) lookup train_data test_data) labels features train_data seed_dl init_dl max_iters smoothing threshold cautious combine propagater prop_mu prop_nu prop_iters
	with Arg_error ->
		Printf.eprintf "usage: %s LABELS FEATURES TRAIN-DATA TEST-DATA SEED-DL INIT-DL MAX-ITERS SMOOTHING THRESHOLD CAUTIOUS COMBINE PROP-TYPE PROP-SUBTYPE PROP-MU PROP-NUN PROP-ITERS\n" Sys.argv.(0);
		exit 1
