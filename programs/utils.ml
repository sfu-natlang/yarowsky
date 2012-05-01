(*
	Various utilities.
*)

let ( & ) f x = f x
let ( % ) f g x = f (g x)
let id x = x
let flip f x y = f y x

let option f = function
	| Some x -> f x
	| None -> ()

let setup x f =
	f x;
	x
let setup_ref x f =
	let y = ref x in
		f y;
		!y

let bound a b x = max a (min x b)
let round x = int_of_float & floor & x +. 0.5
let sign x = if x < 0.0 then -1 else if x > 0.0 then 1 else 0

let sumi f = List.fold_left (fun t x -> t + f x) 0
let sumf f = List.fold_left (fun t x -> t +. f x) 0.0
let prodi f = List.fold_left (fun t x -> t * f x) 1
let prodf f = List.fold_left (fun t x -> t *. f x) 1.0
let mini f = function
	| [] -> raise Not_found
	| xs -> List.fold_left (fun b x -> min b (f x)) max_int xs
let minf f = function
	| [] -> raise Not_found
	| xs -> List.fold_left (fun b x -> min b (f x)) infinity xs
let maxi f = function
	| [] -> raise Not_found
	| xs -> List.fold_left (fun b x -> max b (f x)) min_int xs
let maxf f = function
	| [] -> raise Not_found
	| xs -> List.fold_left (fun b x -> max b (f x)) (-.infinity) xs
let argmini f = function
	| [] -> raise Not_found
	| xs -> snd (List.fold_left (fun (bs, bx as b) x -> let s = f x in if s < bs then (s, x) else b) (max_int, List.hd xs) xs)
let argminf f = function
	| [] -> raise Not_found
	| xs -> snd (List.fold_left (fun (bs, bx as b) x -> let s = f x in if s < bs then (s, x) else b) (infinity, List.hd xs) xs)
let argmaxi f = function
	| [] -> raise Not_found
	| xs -> snd (List.fold_left (fun (bs, bx as b) x -> let s = f x in if s > bs then (s, x) else b) (min_int, List.hd xs) xs)
let argmaxf f = function
	| [] -> raise Not_found
	| xs -> snd (List.fold_left (fun (bs, bx as b) x -> let s = f x in if s > bs then (s, x) else b) (-.infinity, List.hd xs) xs)

let make_with start finish (f : 'a -> 'b) x =
	let y = start x in
		begin try
			let z = f y in
				finish y;
				z
		with e ->
			finish y;
			raise e
		end

let open_in_with f = make_with (fun fn -> if fn = "-" then stdin else open_in fn) close_in f
let open_out_with f = make_with (fun fn -> if fn = "-" then stdout else open_out fn) close_out f
let lines_to_list f ic =
	let xs = ref [] in
		begin try
			while true do
				let l = input_line ic in
				let m = (String.length l) - 1 in
					xs := f (if m > 0 && l.[m] == '\r' then String.sub l 0 m else l) :: !xs
			done
		with End_of_file -> ()
		end;
		List.rev !xs

exception Arg_error

let arg_popper argv =
	let i = ref 1 in
	let pop () = try let x = argv.(!i) in i := !i + 1; x with Invalid_argument _ -> raise Arg_error in
	let peek () = try argv.(!i) with Invalid_argument _ -> raise Arg_error in
	let left () = Array.sub argv !i (Array.length argv - !i) in
		pop, peek, left
let get_opts flag_opts arg_opts argv =
	let module M = struct exception Done end in
	let i = ref 1 in
	let opts = ref [] in
		begin try
			while !i < Array.length argv do
				let key = String.sub argv.(!i) 1 (String.length argv.(!i) - 1) in
					if List.mem key flag_opts then
						opts := (key, "true")::!opts
					else if List.mem key arg_opts then
						try
							let arg = i := !i + 1; argv.(!i) in
								opts := (key, arg)::!opts
						with Invalid_argument _ -> raise Arg_error
					else raise M.Done;
					i := !i + 1
			done;
		with M.Done -> ()
		end;
		Array.concat [[|argv.(0)|]; Array.sub argv !i (Array.length argv - !i)], !opts
let check_arg_num ?(min=0) ?max argv =
	if Array.length argv < min then raise Arg_error;
	match max with None -> () | Some max -> if Array.length argv > max then raise Arg_error

module List =
struct
	include List

	let init n f =
		let rec run i =
			if i < n then f i :: run (i + 1)
			else [] in
			run 0
	let make n x = init n (fun _ -> x)
	let iteri f l =
		let i = ref 0 in
			iter (fun x ->
				f !i x;
				i := !i + 1
			) l
	let rec take n list =
		if n == 0 then []
		else match list with
			| x::xs -> x :: (take (n - 1) xs)
			| [] -> raise (Failure "take")
	let rec take_upto n list =
		if n == 0 then []
		else match list with
			| x::xs -> x :: (take_upto (n - 1) xs)
			| [] -> []

	let uniq xs =
		let used = ref [] in
			List.filter (fun x -> if List.mem x !used then false else (used := x::!used; true)) xs
	let uniqq xs =
		let used = ref [] in
			List.filter (fun x -> if List.memq x !used then false else (used := x::!used; true)) xs

	let mapi f l =
		let i = ref (-1) in
			List.map (fun x -> i := !i + 1; f !i x) l

	let rec iteri2 ?(i0=0) f l1 l2 =
		match l1, l2 with
		| [], [] -> ()
		| x1::x1s, x2::x2s -> f i0 x1 x2; iteri2 ~i0:(i0 + 1) f x1s x2s
		| _ -> raise (Invalid_argument "List.iteri2")
	let rec iter3 f l1 l2 l3 =
		match l1, l2, l3 with
		| [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s -> f x1 x2 x3; iter3 f x1s x2s x3s
		| _ -> raise (Invalid_argument "List.iter3")
	let rec iteri3 ?(i0=0) f l1 l2 l3 =
		match l1, l2, l3 with
		| [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s -> f i0 x1 x2 x3; iteri3 ~i0:(i0 + 1) f x1s x2s x3s
		| _ -> raise (Invalid_argument "List.iteri3")
	let rec iter4 f l1 l2 l3 l4 =
		match l1, l2, l3, l4 with
		| [], [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s, x4::x4s -> f x1 x2 x3 x4; iter4 f x1s x2s x3s x4s
		| _ -> raise (Invalid_argument "List.iter4")
	let rec iteri4 ?(i0=0) f l1 l2 l3 l4 =
		match l1, l2, l3, l4 with
		| [], [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s, x4::x4s -> f i0 x1 x2 x3 x4; iteri4 ~i0:(i0 + 1) f x1s x2s x3s x4s
		| _ -> raise (Invalid_argument "List.iteri4")
	let rec iter5 f l1 l2 l3 l4 l5 =
		match l1, l2, l3, l4, l5 with
		| [], [], [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s, x4::x4s, x5::x5s -> f x1 x2 x3 x4 x5; iter5 f x1s x2s x3s x4s x5s
		| _ -> raise (Invalid_argument "List.iter5")
	let rec iteri5 ?(i0=0) f l1 l2 l3 l4 l5 =
		match l1, l2, l3, l4, l5 with
		| [], [], [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s, x4::x4s, x5::x5s -> f i0 x1 x2 x3 x4 x5; iteri5 ~i0:(i0 + 1) f x1s x2s x3s x4s x5s
		| _ -> raise (Invalid_argument "List.iteri5")
	let rec iter6 f l1 l2 l3 l4 l5 l6 =
		match l1, l2, l3, l4, l5, l6 with
		| [], [], [], [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s, x4::x4s, x5::x5s, x6::x6s -> f x1 x2 x3 x4 x5 x6; iter6 f x1s x2s x3s x4s x5s x6s
		| _ -> raise (Invalid_argument "List.iter6")
	let rec iteri6 ?(i0=0) f l1 l2 l3 l4 l5 l6 =
		match l1, l2, l3, l4, l5, l6 with
		| [], [], [], [], [], [] -> ()
		| x1::x1s, x2::x2s, x3::x3s, x4::x4s, x5::x5s, x6::x6s -> f i0 x1 x2 x3 x4 x5 x6; iteri6 ~i0:(i0 + 1) f x1s x2s x3s x4s x5s x6s
		| _ -> raise (Invalid_argument "List.iteri6")
end

module Array =
struct
	include Array

	let sumi f = fold_left (fun t x -> t + f x) 0
	let sumf f = fold_left (fun t x -> t +. f x) 0.0
	let prodi f = fold_left (fun t x -> t * f x) 1
	let prodf f = fold_left (fun t x -> t *. f x) 1.0

	module Matrix =
	struct
		let make = Array.make_matrix
		let init dimx dimy f =
			Array.init dimx (fun x -> Array.init dimy (fun y -> f x y))
		let iteri f matrix =
			Array.iteri (fun i row ->
				Array.iteri (fun j x ->
					f i j x
				) row
			) matrix
		let copy matrix = Array.map (Array.copy) matrix
		let map f matrix =
			Array.map (fun row ->
				Array.map (fun x ->
					f x
				) row
			) matrix
		let mapi f matrix =
			Array.mapi (fun i row ->
				Array.mapi (fun j x ->
					f i j x
				) row
			) matrix
		let dim matrix =
			let m = Array.length matrix in
			let n = Array.length matrix.(0) in
				assert (Array.fold_left (fun b r -> b && Array.length r == n) true matrix);
				m, n
	end
end

module Iter =
struct
	let fold f x0 (iter : ('a -> unit) -> unit) =
		let x = ref x0 in
			iter (fun y ->
				x := f !x y
			);
			!x
	let to_list iter = List.rev & fold (fun xs x -> x :: xs) [] iter
end
