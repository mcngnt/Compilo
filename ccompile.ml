(* TODO :
- Change lastaddr to using static variables LVALUE_ADDR
- Make LVALUE_ADDR special var
- ADD CALL
- Properly add args to env for accessing
 *)



open Ctable
open Cast

exception Error of string

let hexstring_of_int a = Printf.sprintf "x%x" a


(* Inserts new variable in the table with its name, address and scope *)
let insert_var_tab tab s r isglob = match isglob with
	| true -> SVAR(s,r, isglob)::tab
	| false -> SVAR(s,-r, isglob)::tab


let insert_fun_tab tab s vl = 
	let rec aux l = match l with
		| [] -> []
		| h::q -> begin match h with
								| CDECL(l,s,t) ->
								 s::(aux q)
								| _ -> raise (Error("Function defined in another."))
							end
	in
	let sl = aux vl in SFUN(s,sl)::tab

let rec get_args tab sx = match tab with
	| [] -> raise (Error("Function not found."))
	| SFUN(s,args)::t when s=sx -> args
	| h::t -> get_args t sx


(* Gets the address of the most recent variable whit name s *)
let get_addr_tab tab sx = 
	let rec aux ctab = match ctab with
		| [] -> raise (Error("Variable undefined"))
		| SVAR(s,r,b)::q when s=sx -> r,b
		| h::q -> aux q
	in
		aux tab



let fill_glob_var l = 
	let rec aux li = match li with 
		| []-> ""
		| h::q when h = "LVALUE_ADDR" || h = "LVALUE_ISGLOBAL" -> (aux q) ^ h ^ " .BLKW #1\n"
		| h::q ->  (aux q) ^ "V_" ^ h ^ " .BLKW #1\n"
	in
	"STATIC_VAR\n" ^ (aux l)

let rec insert_no_double l x = match l with
	| [] -> [x]
	| h::q when h = x -> h::q
	| h::q -> h::(insert_no_double q x)




let print_cst_fill id x = let idstring = string_of_int id in 
	"BR IGNORE_CST" ^ idstring ^  "\nCST" ^ idstring ^ " .FILL #" ^ (string_of_int x) ^ "\nIGNORE_CST" ^ idstring ^ "\n"


let gen_condition e ctrue cfalse id flagname brtype = let idstring = string_of_int id in
	e ^ "GOTOC_" ^ brtype ^ " " ^ flagname ^ "_" ^ "ELSE" ^ idstring ^ "\n" ^ ctrue ^ "GOTO " ^ flagname ^ "_" ^ "ENDELSE" ^ idstring ^ "\n" ^ flagname ^ "_" ^  "ELSE" ^ idstring ^ "\n" ^ cfalse ^ flagname ^ "_" ^  "ENDELSE" ^ idstring ^ "\n"

let negate_r0 () = "NOT R0 R0\nADD R0 R0 #1 ;  R0 <- -R0\n"

let print_mult_fun () = "FUN_MULT\nADD R0 R0 #0\nBRn MULT_CHANGE_SIGN\nBR MULT_INIT\nMULT_CHANGE_SIGN\nNOT R0 R0\nADD R0 R0 #1\nNOT R1 R1\nADD R1 R1 #1\nMULT_INIT\nAND R2 R2 #0\nADD R2 R2 R0\nAND R0 R0 #0\nMULT_LOOP\nADD R0 R0 R1\nADD R2 R2 #-1\nBRz MULT_STOP\nBR MULT_LOOP\nMULT_STOP\nRET\n"

let print_div_fun () = "FUN_DIV\nAND R2 R2 #0 ; Q\nLD R2 DIV_ISNEG ; Set is_neg to 0\nADD R1 R1 #0\nBRn DIV_A_NEG\nBR DIV_A_POS\nDIV_A_NEG\nNOT R1 R1 ; Change A sign when negative\nADD R1 R1 #1\nADD R0 R0 #0\nBRn DIV_AN_BN\nBR DIV_AN_BP\nDIV_A_POS\nADD R0 R0 #0\nBRn DIV_AP_BN\nBR DIV_AP_BP\nDIV_AN_BN\nNOT R0 R0\nADD R0 R0 #1\nBR DIV_POS\nDIV_AN_BP\nBR DIV_NEG\nDIV_AP_BN\nNOT R0 R0\nADD R0 R0 #1\nBR DIV_NEG\nDIV_AP_BP\nBR DIV_POS\nDIV_NEG\nADD R3 R2 #1\nST R3 DIV_ISNEG ; IS_NEG <- 1\nBR DIV_ENDSIGN\nDIV_POS\nBR DIV_ENDSIGN\nDIV_ENDSIGN\n; Compute the sign of the result in DIV_ISNEG\nDIV_LOOP\nNOT R3 R1\nADD R3 R3 #1\nADD R3 R3 R0 ; R3 <- R0 - R1 = B - R\nBRp DIV_END_LOOP\nAND R3 R3 #0\nADD R3 R3 R0\nNOT R3 R3\nADD R3 R3 #1 ; R3 <- -B\nADD R1 R1 R3 ; R <- R - B\nADD R2 R2 #1\nBR DIV_LOOP\nDIV_END_LOOP\nADD R0 R2 #0\nLD R3 DIV_ISNEG\nBRz DIV_END\nNOT R0 R0\nADD R0 R0 #1\nDIV_END\nRET\nDIV_ISNEG .BLKW #1\n"

let print_mod_fun () = "FUN_MOD\nAND R2 R2 #0 ; Q\nADD R1 R1 #0\nBRn MOD_A_NEG\nBR MOD_ENDSIGN\nMOD_A_NEG\nADD R1 R1 R0\nBRn MOD_A_NEG\nMOD_ENDSIGN\n; Compute the sign of the result in MOD_ISNEG\nMOD_LOOP\nNOT R3 R1\nADD R3 R3 #1\nADD R3 R3 R0 ; R3 <- R0 - R1 = B - R\nBRp MOD_END_LOOP\nAND R3 R3 #0\nADD R3 R3 R0\nNOT R3 R3\nADD R3 R3 #1 ; R3 <- -B\nADD R1 R1 R3 ; R <- R - B\nADD R2 R2 #1\nBR MOD_LOOP\nMOD_END_LOOP\nADD R0 R1 #0\nMOD_END\nRET\n"

let inverse_condition s = match s with
	| "z" -> "np"
	| "n" -> "zp"
	| "p" -> "nz"
	| "zp" -> "n"
	| "nz" -> "p"
	| "np" -> "z"
	| _ -> raise (Error("Can't find an inverse condition for condition : " ^ s))


let get_var a isglob = 
	(if isglob then "LDR R0 R4 #" ^ (string_of_int a) ^ "\n" else "LDR R0 R5 #" ^ (string_of_int a) ^ "\n")

let set_var a isglob =
	(if isglob then "STR R0 R4 #" ^ (string_of_int a) else "STR R0 R5 #" ^ (string_of_int a) ) ^ "\n"

let set_lvalue_addr a isglob = 
	int isglobint = match isglob with | true -> 1 | false -> 0 in
	incr_flag();
	let set_addr = "LD R0 CST" ^ (string_of_int !flagcount) ^ "\nSTR R0 R4 #0" ^ (print_cst_fill !flagcount a) in
	incr_flag();
	let set_isglob = "LD R0 CST" ^ (string_of_int !flagcount) ^ "\nSTR R0 R4 #1" ^ (print_cst_fill !flagcount isglobint) in
	set_addr ^ set_isglob

let set_lvalue_addr () = 
	incr_flag();
	"\nSTR R0 R4 #0" ^ (print_cst_fill !flagcount a)



let check_file f =
  let orig = 0x3000 in
  let stack = 0xFDFF in
  let flagcount = ref 0 in
  let globvar = ref ["LVALUE_ADDR"; "LVALUE_ISGLOBAL"] in
  let lastaddr = ref (0,false) in

  let incr_flag () = flagcount := !flagcount + 1 in


  let fun_accessing asm = 
		let asml = String.split_on_char '\n' asm in

		let rec find_pos sx env = match env with
			| [] -> raise (Error("Label " ^ sx ^ " not defined."))
			| (s,n)::t when s = sx -> n
			| h::t -> find_pos sx t
		in

		let rec forget_comments li = match li with
			| [] -> []
			| h::t when h = ";" -> []
			| h::t -> h::(forget_comments t)
		in

		let rec create_env l count = match l with
			| [] -> []
			| h::t -> let nbargs = List.length (forget_comments (String.split_on_char ' ' h)) in
								let decompunderscore = String.split_on_char '_' (List.hd (String.split_on_char ' ' h)) in
								begin  match List.hd decompunderscore with
									| ".ORIG" -> create_env t count
									| "GOTORET" -> create_env t (count + 4)
									| "GOTO" -> create_env t (count + 4)
									| "GOTOC" -> create_env t (count + 5)
									| "GLEA" -> create_env t (count + 3)
									| "RET" | "RTI" -> create_env t (count + 1)
									| _ when nbargs = 1 -> (h,count)::(create_env t count)
									| _ when nbargs = 0 -> create_env t count
									| _ -> create_env t (count + 1)
							  end
			in

		let print_jump h env jmp = 
			let n = find_pos (List.nth (String.split_on_char ' ' h) 1) env in
			incr_flag();
			"LD R3 CST" ^ (string_of_int !flagcount) ^ "\n" ^ jmp ^ " R3 ; Jump to label " ^ ( List.nth (String.split_on_char ' ' h) 1 ) ^ "\n" ^ (print_cst_fill !flagcount n)
		in

		let rec print_env env = match env with
			| [] -> "; --DEBUG---\n"
			| (s,c)::t -> "; " ^ s ^ " " ^ (hexstring_of_int c) ^ "\n" ^ (print_env t)
		in

		let rec replace_fun l env = match l with
			| [] -> ""
			| h::t -> let decompunderscore = String.split_on_char '_' (List.hd (String.split_on_char ' ' h)) in
								begin match List.hd (String.split_on_char ' ' h) with
									| "GOTORET" -> let jmpmsg  = print_jump h env "JSRR" in jmpmsg ^ (replace_fun t env)
									| "GOTO" -> let jmpmsg  = print_jump h env "JMP" in jmpmsg ^ (replace_fun t env)
									| _ when List.hd decompunderscore = "GOTOC" -> let jmpmsg  = print_jump h env "JMP" in
															incr_flag();
															 let condjmp = "BR" ^ (inverse_condition (List.nth decompunderscore 1)) ^ " IGNORE_GOTO" ^ (string_of_int !flagcount) ^ "\n" ^ jmpmsg ^ "IGNORE_GOTO" ^ (string_of_int !flagcount) ^ "\n" in
															 condjmp ^ (replace_fun t env)
									| "GLEA" -> let n = find_pos (List.nth (String.split_on_char ' ' h) 2) env in incr_flag();
															let msg = "LD " ^ (List.nth (String.split_on_char ' ' h) 1) ^ " CST" ^ (string_of_int !flagcount) ^ "\n" ^ (print_cst_fill !flagcount n) in
															msg ^ (replace_fun t env)
									| _ -> h ^ "\n" ^ (replace_fun t env)
								end
		in
		(* asm *)
		let env = create_env asml orig in
		(replace_fun asml env) ^ (print_env env)
	in

(*TODO : add local variables to the environment *)
	(* let rec gen_call s lle tab n k = match lle with
			(* I limit functions to 15 args *)
			| [] -> "GOTORET FUN_USER_" ^ s ^ "\nADD R6 R6 #" ^ (string_of_int n)
			| h::t -> let msge = handle_expr h tab in
								msge ^ "ADD R6 R6 #-1\nLDR R0 R6 #0\n" ^ (gen_call s t tab n (k+1))
	 *)

	let rec handle_expr le tab = match le with | (l,e) -> begin match e with
		| VAR s -> let a,isglob = get_addr_tab tab s in (get_var a isglob) ^ (set_lvalue_addr a isglob) (* Set LVALUE_VAR to a and isglob *)
		| CST x -> incr_flag() ; "LD R0 CST" ^ (string_of_int !flagcount) ^ " ; R0 <- cst " ^ (string_of_int x) ^ "\n" ^ (print_cst_fill !flagcount x)
		| NULLPTR -> "AND R0 R0 #0\n"
		| SET_VAR(s,le) -> let a,isglob = get_addr_tab tab s in (handle_expr le tab) ^ (set_var a isglob)
		| SET_VAL(s,le) -> let a,isglob = get_addr_tab tab s in (handle_expr le tab) ^ (if isglob then "LDR R0 R4 #" ^ (string_of_int a) ^ "\n" ^ "ADD R1 R0 R4\nSTR R0 R1 #0" else "LDR R0 R5 #" ^ (string_of_int a) ^ "\n" ^ "ADD R1 R0 R5\nSTR R0 R1 #0" ) ^ " ; (variable " ^ s ^ ")* <- R0\n"
		(* | CALL(s,lle) -> gen_call s lle tab (List.length lle) 0 *)
		| OP1(op, le) -> let msg = (handle_expr le tab) in
										 let a,isglob = !lastaddr in msg ^ begin
											match op with
												| M_MINUS -> negate_r0()
												| M_NOT -> "NOT R0 R0\n"
												| M_ADDR ->  incr_flag(); "LD R0 CST" ^ (string_of_int !flagcount) ^ "\n" ^ (print_cst_fill !flagcount a)
												| M_DEREF -> if isglob then "ADD R1 R4 R0\nLDR R0 R1 #0" else "ADD R1 R5 R0\nLDR R0 R1 #0\n" ^ set_lvalue_addr() (* set LVALUE_VAR to value of R0 *)
												| M_PRE_INC -> let getmsg = (get_var a isglob) and setmsg = (set_var a isglob) in getmsg ^ "ADD R0 R0 #1\n" ^ setmsg (*fetch address from LVALUE_VAR*)
												| M_PRE_DEC -> let getmsg = (get_var a isglob) and setmsg = (set_var a isglob) in getmsg ^ "ADD R0 R0 #-1\n" ^ setmsg
												| M_POST_INC -> let getmsg = (get_var a isglob) and setmsg = (set_var a isglob) in getmsg ^ "ADD R0 R0 #1\n" ^ setmsg ^ "ADD R0 R0 #-1\n"
												| M_POST_DEC -> let getmsg = (get_var a isglob) and setmsg = (set_var a isglob) in getmsg ^ "ADD R0 R0 #-1\n" ^ setmsg ^ "ADD R0 R0 #1\n"
											end
		| OP2(op, le1, le2) -> (handle_expr le1 tab) ^ "STR R0 R6 #0 ; Store R0 on the stack\nADD R6 R6 #-1 ; Increase the stack\n" ^ (handle_expr le2 tab) ^ "ADD R6 R6 #1 ; Decrease the stack\nLDR R1 R6 #0 ; Retrieve upmost result on the stack in R1\n" ^ begin
													 match op with
													 	| S_ADD -> "ADD R0 R0 R1 ; R0 <- R0 + R1\n"
													 	| S_SUB -> (negate_r0()) ^ "ADD R0 R0 R1 ; R0 <- R1 - R0\n"
													 	| S_MUL -> "GOTORET FUN_MULT\n"
													 	| S_DIV -> "GOTORET FUN_DIV\n"
													 	| S_MOD -> "GOTORET FUN_MOD\n"
													 end
		| CMP(op, le1, le2) -> (handle_expr le1 tab) ^ "STR R0 R6 #0 ; Store R0 on the stack\nADD R6 R6 #-1 ; Increase the stack\n" ^ (handle_expr le2 tab) ^ "ADD R6 R6 #1 ; Decrease the stack\nLDR R1 R6 #0 ;  Retrieve upmost result on the stack in R1\n" ^
													 let brtype = match op with
													 										| C_LT -> "zp"
													 										| C_LE -> "p"
													 										| C_EQ -> "np"
													 							in 
													 let e = (negate_r0()) ^ "ADD R0 R0 R1 ; Compute R1 - R0 for comparison\n" in
													 let ctrue = "AND R0 R0 #0\nADD R0 R0 #1 ; If comparison true, set R0 to 1\n" in 
													 let cfalse = "AND R0 R0 #0 ; If comparison false, set R0 to 0\n" in
													 incr_flag();
													 (gen_condition e ctrue cfalse !flagcount "CMP" brtype) ^ "; R0 <-  e1 " ^ begin match op with
													 																																					 			| C_LT -> ">"
													 																																					 			| C_LE -> "<"
													 																																					 			| C_EQ -> "=="
													 																																					 end ^ " e2\n"
		| EIF(le1,le2,le3) -> incr_flag(); let e = (handle_expr le1 tab) in let c1 = handle_expr le2 tab in let c2 = handle_expr le2 tab in gen_condition e c1 c2 !flagcount "IF" "z"
		| ESEQ(lle) -> List.fold_left (fun acc le -> acc ^ (handle_expr le tab)) "" lle
		| _ -> ""
	end
	in

	 let rec handle_block vdl lcl tab r6 = match vdl with
		| [] -> begin match lcl with 
					| [] -> ""
					| h::q -> (handle_code h tab r6) ^ (handle_block [] q tab r6)
				end
		| h::q -> begin match h with
							| CDECL(l,s,t) -> "ADD R6 R6 #-1 ; Add variable " ^ s ^ " to the stack \n" ^ handle_block q lcl ( insert_var_tab tab s r6 false ) (r6+1)
							| _ -> ""
						end
	and handle_code lc tab r6 = match lc with | (l,c) -> begin match c with
	 			| CBLOCK(vdl, lcl) -> handle_block vdl lcl tab r6
				| CEXPR le -> handle_expr le tab
				| CIF(le, lc1,lc2) -> incr_flag(); let e = (handle_expr le tab) in let c1 = (handle_code lc1 tab r6) in let c2 = (handle_code lc2 tab r6) in gen_condition e c1 c2 !flagcount "IF" "z"
				| CWHILE(le, lc) -> let flagstring = string_of_int (!flagcount) in incr_flag(); "STARTWHILE" ^ flagstring ^ "\n" ^ (handle_expr le tab) ^ "BRz ENDWHILE" ^ flagstring ^ "\n" ^ (handle_code lc tab r6) ^ "BR STARTWHILE" ^ flagstring ^ "\n" ^ "ENDWHILE" ^ flagstring ^ "\n"
				| CRETURN (Some le) -> handle_expr le tab
				| _ -> ""
		end
	in


	 let rec handle_val_dec f tab r4 = match f with
	 	| [] -> ""
	 	| CDECL(l,s,t)::q -> globvar := s::(!globvar) ; ( handle_val_dec q (insert_var_tab tab s r4 true) (r4 + 1))
	 	| CFUN(l,s,vl,t,lc)::q -> let funtab = (insert_fun_tab tab s vl) in
	 														let funbase = "FUN_USER_" ^ s ^ "\nADD R6 R6 #-1\nLDR R5 R6 #0 ; Store R5 on the stack\nADD R6 R6 #-1\nLDR R7 R6 #0 ; Store R7 on the stack\nADD R6 R6 #-1\nAND R5 R5 #0\nADD R5 R5 R6 ; R5 <- R6\n" in
	 														let c = (handle_code lc funtab 0) in
	 														let endfun = "LDR R7 R5 #-1 ; Restore R7\nLDR R5 R5 #-2 ; Restore R5\n" in
	 														funbase ^ c ^ endfun ^ (handle_val_dec q funtab r4)
	in

	let codebody = handle_val_dec f [] 0 in

	incr_flag();
	let header = ".ORIG " ^ (hexstring_of_int orig) ^ "\nLD R6 CST" ^ (string_of_int !flagcount) ^"\n" ^ (print_cst_fill !flagcount stack) ^ "AND R5 R5 #0\nADD R5 R5 R6\nGLEA R4 STATIC_VAR\nADD R4 R4 #2\nGOTO FUN_USER_main\n" in
	let rawasm = header
		^ print_mult_fun()
		^ print_div_fun()
		^ print_mod_fun()
	  ^ codebody
	  ^ (fill_glob_var !globvar) ^
	  ".END" in
	let funasm = fun_accessing rawasm in
	funasm

