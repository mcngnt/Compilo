
open Cast
open Tast




let rec insert_no_double l x = 
	let sx = match x with |VART(s,t) -> s |FUNT(s,vl,t) -> s in
	let rec aux li = match li with
		| [] -> [x]
		| VART(s,t)::q when s = sx -> failwith "Variable déja définie"
		| FUNT(s,vl,t)::q when s=sx -> failwith "Fonction déja définie"
		| h::q -> aux q
	in
	aux l
;;

let rec handle_code c env = match c with
	| 
;;

let rec handle_block b env = match b with
	| [], [] -> ()
	| [], h::q -> match h with | (l,c) ->  handle_code c env; handle_block ([],q) env
	| h::q, lcl -> match h with | CDECL(l,s,t) -> handle_block (q,lcl) (insert_no_double env (VART(s,t))) | _ -> failwith "Fonction définie dans une autre"
;;


let rec convert_to_type l = match l with
	| [] -> []
	|CDECL(l,s,t)::q -> t::(convert_to_type q)
	| _ -> failwith "Fonction en argument d'une autre"
;;


let rec handle_val_dec f env = match f with
	| [] -> []
	| CDECL(l,s,t)::q ->  handle_val_dec q (insert_no_double env (VART(s,t)))
	| CFUN(l,s,vl,t,lc)::q -> let newenv = insert_no_double env (FUNT(s, convert_to_type vl, t)) in handle_block (match lc with | (l,c) -> c) newenv ; handle_val_dec q newenv
;;


let check_file f = handle_val_dec f [];;