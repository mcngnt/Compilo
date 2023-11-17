

let print_hex a = Printf.sprintf "x%x\n" a

let rec insert_tab tab x l = 
	let sx = match x with |VART(s,t,d) -> s |FUNT(s,vl,t) -> s in
	let dx = match x with |VART(s,t,d) -> d |FUNT(s,vl,t) -> 0 in
	let rec aux li = match li with
		| [] -> [x]
		| VART(s,t,d)::q when s = sx && (d = dx || d = 0) -> raise (Error(l, "Variable " ^ sx ^ " is already defined."))
		| FUNT(s,vl,t)::q when s=sx -> raise (Error(l, "Function " ^ sx ^ " is already defined."))
		| h::q -> h::(aux q)
	in
	aux env


let rec find_address tab s glob = match tab with
  | ()

let handle_val_dec f tab = match f with
  | CDECL(l,s,t)::q ->  handle_val_dec q (insert_env env (VART(s,convert_ttype t,0)) l)

let check_file f = 
  let orig = x3000 in
  (print_hex orig) ^ handle_val_dec f (x5000,) ^ ".END"



