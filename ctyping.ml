
open Cast
open Tast

exception Error of location * string



let rec insert_no_double env x = 
	let sx = match x with |VART(s,t) -> s |FUNT(s,vl,t) -> s in
	let rec aux li = match li with
		| [] -> [x]
		| VART(s,t)::q when s = sx -> failwith "Variable déja définie"
		| FUNT(s,vl,t)::q when s=sx -> failwith "Fonction déja définie"
		| h::q -> aux q
	in
	aux env

let rec get_type_env env sx l = match env with
	| VART(s,t)::q when s = sx -> t
	| FUNT(s,vl, t)::q when s = sx -> t
	| h::q -> get_type_env q sx l
	| [] -> raise (Error(l, "Variable " ^ sx ^ " not defined."))

let rec handle_expr le env = match le with | (l,e) -> begin match e with
	| VAR s -> get_type_env env s l
	| CST x -> TINT
	| NULLPTR -> TPTR TINT
	| _ -> TINT
end


let rec handle_block vdl lcl env = match vdl with
	| [] -> begin match lcl with 
				| [] -> ()
				| h::q -> match h with
							| (l,c) -> handle_code c env; handle_block [] q env
			end
	| h::q -> match h with
						| CDECL(l,s,t) -> handle_block q lcl ( insert_no_double env (VART(s,t)) ) 
						| CFUN(l,s,vl,t,lc) -> 
						raise (Error(l, "Function defined in another."))

and handle_code c env = match c with
	| CBLOCK(vdl, lcl) -> handle_block vdl lcl env
	| CEXPR le -> let _ = handle_expr le env in ()
	| _ -> ()




let rec convert_to_type l = match l with
	| [] -> []
	|CDECL(l,s,t)::q -> t::(convert_to_type q)
	| CFUN(l,s,vl,t,lc)::q -> raise (Error(l, "Function passed as a parameter."))



let rec handle_val_dec f env = match f with
	| [] -> []
	| CDECL(l,s,t)::q ->  handle_val_dec q (insert_no_double env (VART(s,t)))
	| CFUN(l,s,vl,t,lc)::q -> let newenv = insert_no_double env (FUNT(s, convert_to_type vl, t)) in handle_code (match lc with | (l,c) -> c) newenv ; handle_val_dec q newenv



let check_file f = handle_val_dec f [];;