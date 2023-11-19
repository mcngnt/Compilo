
open Cast
open Tast

exception Error of location * string


(* Inserts a new variable in the environment checking previous variables to ensure that typing rules are respected *)
let rec insert_env env x l = 
	let sx = match x with |VART(s,t,d) -> s |FUNT(s,vl,t) -> s in
	let dx = match x with |VART(s,t,d) -> d |FUNT(s,vl,t) -> 0 in
	let rec aux li = match li with
		| [] -> [x]
																		(*I do not allow to redefine global variables or to redefine a variable in the same scope (ie same depth)*)
		| VART(s,t,d)::q when s = sx && (d = dx || d = 0) -> raise (Error(l, "Variable " ^ sx ^ " is already defined."))
		| FUNT(s,vl,t)::q when s=sx -> raise (Error(l, "Function " ^ sx ^ " is already defined."))
		| h::q -> h::(aux q)
	in
	aux env


(* Converts a ctype into a ttype *)
let rec convert_ttype t = match t with
	| TINT -> TTINT
	| TPTR tx -> TTPTR (convert_ttype tx)

(* Returns a string of the ttype *)
let rec print_type t = match t with
	| TTINT -> "int"
	| TTPTR tx -> "pointer of " ^ (print_type tx)
	| TTNULL -> "null pointer"

(* Returns the string of the ctype associated with a vdeclt *)
let print_vdeclt v = match v with
	| VART(s,t,d) -> s ^ " of type " ^ (print_type t)
	| FUNT(s,vl, t) -> s

(* Checks if two ctype are equal *)
let rec equals_type t1 t2 = match (t1,t2) with
	| TTINT,TTINT -> true
	| TTINT, _ -> false
	| _,TTINT -> false
	| TTNULL, _ -> true
	| _,TTNULL -> true
	| TTPTR st1, TTPTR st2 -> equals_type st1 st2

(* Checks if two ctype are not equal *)
let nequals_type t1 t2 = not (equals_type t1 t2)

(* Gets the type of a variable in the env (the newest variable of the env is used if two variables have the same name) *)
let get_type_env env sx l = 
	let rec aux e = match e with
		| VART(s,t,d)::q when s = sx -> t
		| FUNT(s,vl, t)::q when s = sx -> t
		| h::q -> aux q
		| [] -> raise (Error(l, "Declaration " ^ sx ^ " is not defined."))
	in
	aux (List.rev env)


(* Checks if there is a function with the correct name and arguments as the ones provided *)
let rec check_args_fun_env env sx args l = match env with
	| FUNT(s,vl, t)::q when s = sx && args = vl -> ()
	| h::q -> check_args_fun_env q sx args l
	| [] -> raise (Error(l, "No function named " ^ sx ^ " matches with the args provided."))


(* Gets the type of the latest function added to the environment *)
let  current_fun_type_env env l =
	let rec aux e = match e with
		| FUNT(s,vl, t)::q  -> t
		| h::q -> aux q
		| [] -> raise (Error(l, "There is no function to return from."))
	in
	aux (List.rev env)

(* Checks if a given expression is an left value *)
let rec is_lvalue le = match le with
	| (l,e) -> begin match e with
				| VAR s -> true
				| OP1(op,le) when op = M_DEREF -> is_lvalue le
				| _ -> false
				end

(* Handles expression according to typing rules and returns the type of the expression *)
let rec handle_expr le env = match le with | (l,e) -> begin match e with
	| VAR s -> get_type_env env s l
	| CST x -> TTINT
	| STRING s -> TTPTR TTINT
	| NULLPTR -> TTNULL
	| SET_VAR(s,le) -> (* Here I don't change the type of the variable in the environment, making the statements (int* a; a = NULL; return *a) a valid code
						  as it is the case for gcc *)
						let t = handle_expr le env in begin match get_type_env env s l with
														| tx when equals_type tx t -> t
														| tx -> raise (Error(l, "Variable " ^ s ^ " of type " ^ (print_type tx) ^ " doesn't match expression affectation of type " ^ (print_type t)))
													end
	| SET_VAL(s,le) -> let t = handle_expr le env in begin match get_type_env env s l with
														| TTPTR tx when equals_type tx t -> t
														| _ -> raise (Error(l, "Types do not match."))
													end
	| CALL(s,lle) -> let lt = List.map 	(fun e -> handle_expr e env) lle in check_args_fun_env env s lt l ; get_type_env env s l		
	| OP1(op, le) -> let islv = is_lvalue le in begin match op with
						| M_ADDR | M_POST_INC | M_POST_DEC | M_PRE_INC | M_PRE_DEC when not islv -> raise (Error(l, "The expression is not an lvalue."))
						| _ -> ()
					end;
					begin match handle_expr le env with
						| TTINT -> begin match op with
										| M_DEREF -> raise (Error(l, "Dereferencing int type not allowed."))
										| M_ADDR -> TTPTR TTINT
										| _ -> TTINT
								end
						| TTPTR t -> begin match op with
										| M_MINUS -> raise (Error(l, "Negative pointer not allowed."))
										| M_DEREF -> t
										| M_ADDR -> TTPTR (TTPTR t)
										| _ -> TTPTR t 
								end
						| TTNULL -> begin match op with
										| M_MINUS -> raise (Error(l, "Negative null pointer not allowed."))
										| M_DEREF -> raise (Error(l, "Deref of null pointer not allowed."))
										| M_ADDR -> TTPTR (TTNULL)
										| _ -> TTNULL
								end
					end
	| OP2(op, le1, le2) -> let t1 = handle_expr le1 env in let t2 = handle_expr le2 env in 
							begin match (t1,t2) with
									| (TTINT, TTINT) -> TTINT
									| (TTINT, TTPTR t) -> begin match op with
															| S_ADD -> TTPTR t
															| _ -> raise (Error(l, "Illegal pointer-int operation."))
														end
									| (TTPTR t, TTINT) -> begin match op with
															| S_ADD -> TTPTR t
															| S_SUB -> TTPTR t
															| _ -> raise (Error(l, "Illegal pointer-int operation."))
														end
									| (TTPTR t1, TTPTR t2) when equals_type t1 t2 -> begin match op with
																			| S_SUB -> TTPTR t1
																			| _ -> raise (Error(l, "Illegal pointer-pointer operation."))
																		end
									| (TTINT, TTNULL) -> begin match op with
															| S_ADD -> TTNULL
															| _ -> raise (Error(l, "Illegal null pointer-int operation."))
														end
									| (TTNULL, TTINT) -> begin match op with
															| S_ADD -> TTNULL
															| S_SUB -> TTNULL
															| _ -> raise (Error(l, "Illegal null pointer-int operation."))
														end
									| _ -> raise (Error(l, "Illegal pointer operation with NULL pointer."))
							end
	| CMP(op, le1,le2) -> let t1 = handle_expr le1 env in let t2 = handle_expr le2 env in if nequals_type t1 t2 then raise (Error(l, "Can't compare different types.")); TTINT
	| EIF(le1,le2,le3) -> let t1 = handle_expr le1 env in let t2 = handle_expr le2 env in let t3 = handle_expr le3 env in if (nequals_type t1 TTINT) || (nequals_type t2  t3) then raise (Error(l, "Uncompatible types involved in the ternary operator.")); t2
	| ESEQ lle -> begin match List.rev (List.map (fun e -> handle_expr e env) lle) with (*I check typing rules on every expr and returs the type of the last*)
							| [] -> TTINT
							| h::q -> h
				  end
end


(* handle_block and handle_code return true if there is a return statement in every code branch *)
let rec handle_block vdl lcl env d = match vdl with
	| [] -> begin match lcl with 
				| [] -> false
				| h::q -> (handle_code h env d) || (handle_block [] q env d)
			end
	| h::q -> match h with
						| CDECL(l,s,t) -> handle_block q lcl ( insert_env env (VART(s,convert_ttype t,d)) l ) d
						| CFUN(l,s,vl,t,lc) -> raise (Error(l, "Function defined in another is not allowed."))

and handle_code lc env d = match lc with | (l,c) -> begin match c with
	| CBLOCK(vdl, lcl) -> handle_block vdl lcl env (d+1)
	| CEXPR le -> let _ = handle_expr le env in false
	| CIF(le, lc1, lc2) -> let t = handle_expr le env in if nequals_type t TTINT then raise (Error(l, "Non-int type as condition.")) else (handle_code lc1 env d) && (handle_code lc2 env d)
	| CWHILE(le, lc) -> let t = handle_expr le env in if nequals_type t TTINT then raise (Error(l, "Non-int type as condition.")) else handle_code lc env d
	| CRETURN None -> true
	| CRETURN (Some le) -> let t = handle_expr le env in let ft = current_fun_type_env env l in if nequals_type t ft then raise (Error(l, "Return type " ^ (print_type t) ^ " does not correspond to function type " ^ (print_type ft) ^ ".")); true
end



(* Converts a list of var_declaration to a ctype list *)
let rec convert_to_type_dec l = match l with
	| [] -> []
	|CDECL(l,s,t)::q -> (convert_ttype t)::(convert_to_type_dec q)
	|CFUN(l,s,vl,t,lc)::q -> raise (Error(l, "Function passed as a parameter."))

(* Converts a list of var_declaration to a vdeclt (custom type used in env) list *)
let rec convert_to_vdeclt_dec l = match l with
	| [] -> []
	| CDECL(l,s,t)::q -> VART(s,convert_ttype t,1)::(convert_to_vdeclt_dec q)
	| CFUN(l,s,vl,t,lc)::q -> raise (Error(l, "Function passed as a parameter."))

(* Function to handle global declarations *)
let rec handle_val_dec f env = match f with
	| [] -> ()
	| CDECL(l,s,t)::q ->  handle_val_dec q (insert_env env (VART(s,convert_ttype t,0)) l)
	| CFUN(l,s,vl,t,lc)::q -> let newenv = insert_env env (FUNT(s, convert_to_type_dec vl, convert_ttype t)) l in let b = handle_code lc (List.fold_left (fun e x -> insert_env e x l)  newenv (convert_to_vdeclt_dec vl) ) 1 in if not b then raise (Error(l, "Function " ^ s ^ " doesn't return in every code branch.")); handle_val_dec q newenv


(* Starts the AST traversing with an empty environment *)
(* Here, i don't return a typed AST because it is not needed in LC3 compilation (though I ankowledge that it is very
useful in general, such as when dealing with variable-sized types).
However, I still check every typing rule during the AST traversal. 
(My typing code was already completed when I realized the need to return a typed AST. I decided not to change it
because my code would become much longer and less elegant if I did so, 
especially considering that I already use the return types of functions to check typing rules) *)
let check_file f = handle_val_dec f [];;

(* Here I represnt the environment as a list of variablles and functions declaration, the environment beign updated
when passed as a parameter to recursive functions. *)