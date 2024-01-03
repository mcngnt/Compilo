open Ctable
open Cast

exception Error of string

(* Handle special characters in string *)
let ignore_specials s = 
	let rec aux l = match l with
		| [] -> ""
		| h::t when h = '\n' -> "\\n" ^ (aux t)
		| h::t when h = '\"' -> "\\\"" ^ (aux t)
		| h::t when h = '\\' -> "\\\\" ^ (aux t)
		| h::t -> (String.make 1 h) ^ (aux t)
	in
	aux (List.init (String.length s) (String.get s))


(* Inserts new variable in the table with its name, absolute offest relative to base and scope *)
let insert_var_tab tab s r isglob = SVAR(s,r, isglob)::tab

(* Inserts a function in the environment *)
let insert_fun_tab tab s vl = 
	let rec convert_decl l = match l with
		| [] -> []
		| h::q -> begin match h with
								| CDECL(l,s,t) ->
								 s::(convert_decl q)
								| _ -> raise (Error("Function defined in another."))
							end
	in
	let rec convert_args l i = match l with
		| [] -> []
		| h::t -> SVAR(h,i,false)::(convert_args t (i-1))
	in
	let sl = convert_decl vl in 
	SFUN(s,sl)::( (convert_args (List.rev sl) (-4)) @ tab)



(* Gets the address of the most recent variable whit name s *)
let get_addr_tab tab sx = 
	let rec aux ctab = match ctab with
		| [] -> raise (Error("Variable " ^ sx ^ " undefined"))
		| SVAR(s,r,b)::q when s=sx -> r,b
		| h::q -> aux q
	in
		aux tab

(* Generates declarations for static variables *)
let fill_glob_var l = 
	let rec aux li = match li with 
		| []-> []
		| h::q ->  ( "V_" ^ h ^ " .BLKW #1" )::(aux q)
	in
	"STATIC_VAR"::(aux (List.rev l))

(* Generate declarations for strings constants encountered during compilation *)
let fill_strings l = 
	let rec aux li = match li with 
		| []-> []
		| (s,label,n)::q ->  ( label ^ " .STRINGZ \"" ^ (ignore_specials s) ^ "\"" )::(aux q)
	in
	"STRINGS"::(aux (List.rev l))

(* Creates a CST label as well as a BR to avoid treating the constant as an instruction *)
let print_cst_fill id x = let idstring = string_of_int id in 
	["BR IGNORE_CST" ^ idstring ; "CST" ^ idstring ^ " .FILL #" ^ (string_of_int x) ; "IGNORE_CST" ^ idstring]

(* LC3 code for arithmetic functions *)
let print_mult_fun () = ["FUN_MULT";"ADD R0 R0 #0";"BRn MULT_CHANGE_SIGN";"BR MULT_INIT";"MULT_CHANGE_SIGN";"NOT R0 R0";"ADD R0 R0 #1";"NOT R1 R1";"ADD R1 R1 #1";"MULT_INIT";"AND R2 R2 #0";"ADD R2 R2 R0";"AND R0 R0 #0";"MULT_LOOP";"ADD R0 R0 R1";"ADD R2 R2 #-1";"BRz MULT_STOP";"BR MULT_LOOP";"MULT_STOP";"RET"]
let print_div_fun () = ["FUN_DIV";"AND R2 R2 #0";"ADD R0 R0 #0";"BRp DIV_NOT_NEG_B";"NOT R0 R0";"ADD R0 R0 #1";"ADD R2 R2 #1";"DIV_NOT_NEG_B";"ADD R1 R1 #0";"BRp DIV_NOT_NEG_A";"NOT R1 R1";"ADD R1 R1 #1";"ADD R2 R2 #1";"DIV_NOT_NEG_A";"ST R2 DIV_NEG_V";"AND R2 R2 #0";"DIV_LOOP";"NOT R3 R1";"ADD R3 R3 #1";"ADD R3 R3 R0";"BRp DIV_END_LOOP";"AND R3 R3 #0";"ADD R3 R3 R0";"NOT R3 R3";"ADD R3 R3 #1";"ADD R1 R1 R3";"ADD R2 R2 #1";"BR DIV_LOOP";"DIV_END_LOOP";"ADD R0 R2 #0";"LD R2 DIV_NEG_V";"ADD R2 R2 #-1";"BRz DIV_NEG_RES";"BR DIV_END";"DIV_NEG_RES";"NOT R0 R0";"ADD R0 R0 #1";"DIV_END";"DIV_NEG_V .BLKW #1";"RET"]
let print_mod_fun () = ["FUN_MOD";"AND R2 R2 #0";"ADD R0 R0 #0";"BRp MOD_NOT_NEG_B";"NOT R0 R0";"ADD R0 R0 #1";"MOD_NOT_NEG_B";"ADD R1 R1 #0";"BRp MOD_NOT_NEG_A";"NOT R1 R1";"ADD R1 R1 #1";"ADD R2 R2 #1";"MOD_NOT_NEG_A";"MOD_LOOP";"NOT R3 R1";"ADD R3 R3 #1";"ADD R3 R3 R0";"BRp MOD_END_LOOP";"AND R3 R3 #0";"ADD R3 R3 R0";"NOT R3 R3";"ADD R3 R3 #1";"ADD R1 R1 R3";"BR MOD_LOOP";"MOD_END_LOOP";"ADD R0 R1 #0";"ADD R2 R2 #0";"BRp MOD_NEG_RES";"BR MOD_END";"MOD_NEG_RES";"NOT R0 R0";"ADD R0 R0 #1";"MOD_END";"RET"]


(* Useful for BR to negative conditions *)
let inverse_condition s = match s with
	| "z" -> "np"
	| "n" -> "zp"
	| "p" -> "nz"
	| "zp" -> "n"
	| "nz" -> "p"
	| "np" -> "z"
	| _ -> raise (Error("Can't find an inverse condition for condition : " ^ s))


(* Base function used to handle the code generation *)
let check_file f =
  let orig = 0x3000 in
  let stack = 0xFDFF in
  (* Counter used to generate labels injectively *)
  let flagcount = ref 0 in
  (* List of every global variables : the list is non-empty because it contains pseudo-variables used to handle lvalues *)
  let globvar = ref [] in
  let strings = ref [] in
  let stringcount = ref 0 in
  let use_mod = ref false in
  let use_mult = ref false in
  let use_div = ref false in

  let incr_flag () = flagcount := !flagcount + 1 in
  let incr_string () = stringcount := !stringcount + 1 in

  (* Generates an if e then ctrue else cfalse statement where is treated differently based on the brtype flag *)
  let gen_condition e ctrue cfalse flagname brtype = 
  	incr_flag();	
  	let idstring = string_of_int !flagcount in
  	let condjmp = ["BR" ^ (inverse_condition brtype) ^ " IGNORE_JMP_IF" ^ idstring ; "GLEA R3 " ^ flagname ^ "_" ^ "ELSE" ^ idstring ; "JMP R3" ; "IGNORE_JMP_IF" ^ idstring] in
  	e @ ["ADD R0 R0 #0"] @ condjmp @ ctrue @ ["GLEA R3 " ^ flagname ^ "_" ^ "ENDELSE" ^ idstring ; "JMP R3" ; flagname ^ "_" ^  "ELSE" ^ idstring] @ cfalse @ [flagname ^ "_" ^  "ENDELSE" ^ idstring]
  in

  let load_immediate r x = match x with
  	| x when Int.abs(x) < 15 -> ["AND " ^ r ^ " " ^ r ^ " #0";"ADD " ^ r ^ " " ^ r ^ " #" ^ (string_of_int x)]
  	| _ -> incr_flag() ; ["LD " ^ r ^ " CST" ^ (string_of_int !flagcount)] @ (print_cst_fill !flagcount x)
  in

  (* Puts the value of a variable in R0 by first storing the address in R1 *)
  (* I store the address in R1 because if I did LDR R0 R4 #offest, I would be limited to 32 offset, 
  limiting the number of static variables to 32 *)
  let get_var a isglob inR0 = 
  	incr_flag();
  	(load_immediate "R1" a) @  (if isglob then ["ADD R1 R4 R1";"LDR R1 R1 #0"] else ["NOT R1 R1";"ADD R1 R1 #1";"ADD R1 R5 R1";"LDR R1 R1 #0"]) @ (if inR0 then ["ADD R0 R1 #0"] else [])
  in

  (* Sets the value of a variable to the content of R0 by first storing the address in R1 *)
  let set_var a isglob =
  	incr_flag();
  	(load_immediate "R1" a) @ (if isglob then ["ADD R1 R4 R1";"STR R0 R1 #0"] else ["NOT R1 R1";"ADD R1 R1 #1";"ADD R1 R5 R1";"STR R0 R1 #0"])
  in


  (* Retreive the absolute address of a variable and place it in R0 *)
  let set_lvalue addr isglob = 
    incr_flag();
    (load_immediate "R0" addr) @ ( if isglob then ["ADD R0 R0 R4"] else ["NOT R0 R0";"ADD R0 R0 #1";"ADD R0 R0 R5"] )
  in

(* Get the length of a string stored in the strings constants *)
  let get_string_len lab = 
  	let rec aux l = match l with
  		| [] -> raise (Error("String " ^ lab ^ " does not exist."))
  		| (_,labx, n)::t when labx = lab -> n
  		|h::t -> aux t
  	in
  	aux !strings
  in

  (* Compute the second pass of compilation, translating made up instruction like GLEA to real LC3 code *)
  let secondpass asm = 

		let rec find_line sx env = match env with
			| [] -> raise (Error("Label " ^ sx ^ " not defined."))
			| (s,n)::t when s = sx -> n
			| h::t -> find_line sx t
		in

		(* Stores every label and string declaration with their line number in the code *)
		let rec create_env l count = match l with
			| [] -> []
			| h::t -> let args = String.split_on_char ' ' h in
								let nbargs = List.length args in
								begin match List.hd args with
									| ".ORIG" -> create_env t count
									| "GLEA" -> create_env t (count + 3)
									| "RET" | "RTI" | "PUTS" | "GETC" | "OUT" | "HALT" -> create_env t (count + 1)
									| _ when nbargs = 1 -> (h,count)::(create_env t count)
									| _ when nbargs > 1 && List.nth args 1 = ".STRINGZ" -> let size = get_string_len (List.hd args) in (List.hd args,count)::(create_env t (count + size + 1))
									| _ -> create_env t (count + 1)
							  end
			in

		(* Replaces GLEA, an instruction used for accessing labels and strings more than 256 lines away, with real LC3 code *)
		let rec replace_fun l env = match l with
			| [] -> []
			| h::t -> let args = String.split_on_char ' ' h in
								begin match List.hd args with
										(* Instruction example -> GLEA RX LABEL where x \in {0,1,2,3} *)
									| "GLEA" -> 
															let label = (List.nth args 2) in
															let r = (List.nth args 1) in
															let n = find_line label env in incr_flag();
															let msg = load_immediate r n in
															msg @ (replace_fun t env)
									| _ -> h :: (replace_fun t env)
								end
		in

		(* Prints every label used and its line number for debug purposes *)
		let rec print_env l = match l with
			| []-> []
			| (s,c)::t -> ( ";  " ^ s ^ " <- " ^ (string_of_int c) ) :: (print_env t)
		in

		let env = create_env asm orig in
		replace_fun asm env @ (print_env env)
	in

(* Used when a function call occurs : push arguments on the stack, call the function and then pop the arguments of the stack *)
	let rec gen_call s lle tab n = match lle with
			| [] -> incr_flag(); ["GLEA R3 FUN_USER_" ^ s ; "JSRR R3"] @ (load_immediate "R1" n) @ ["ADD R6 R6 R1"]
			| h::t -> let msge = handle_expr h tab false in
								msge @ ["STR R0 R6 #0" ; "ADD R6 R6 #-1"] @ (gen_call s t tab n)

	and handle_expr le tab get_lvalue = match le with | (l,e) -> begin match e with
		| VAR s -> let a,isglob = get_addr_tab tab s in if get_lvalue then set_lvalue a isglob  else (get_var a isglob  true)
		| CST x -> load_immediate "R0" x
		| STRING s ->  incr_string(); strings := (s,"STRING" ^ (string_of_int !stringcount), String.length s)::!strings ; ["GLEA R0 STRING" ^ (string_of_int !stringcount)]
		| NULLPTR -> ["AND R0 R0 #0"]
		| SET_VAR(s,le) -> let a,isglob = get_addr_tab tab s in let e = (handle_expr le tab false) in e @ (set_var a isglob )
		| SET_VAL(s,le) -> let a,isglob = get_addr_tab tab s in let e = (handle_expr le tab false) in e @ (get_var a isglob  false) @  ["STR R0 R1 #0"]
		(* Call string related functions *)
		| CALL(s,lle) when s = "puts" -> let e = handle_expr (List.hd lle) tab false in e @ ["PUTS"]
		| CALL(s,lle) when s = "putc" -> let e = handle_expr (List.hd lle) tab false in e @ ["OUT"]
		| CALL(s,lle) when s = "getc" -> ["GETC"]
		(* Call user defined functions *)
		| CALL(s,lle) -> ( gen_call s lle tab (List.length lle) )
		| OP1(op, le) ->  (* Decide depending of the operator whether the value or the adress of the argument is needed *)
											let msg = match op with
																| M_ADDR | M_PRE_INC | M_PRE_DEC | M_POST_INC | M_POST_DEC -> handle_expr le tab true
																| _ -> handle_expr le tab false
											in
										 	msg @ begin
											match op with
												| M_MINUS -> ["NOT R0 R0";"ADD R0 R0 #1"]
												| M_NOT -> ["NOT R0 R0"]
												(* The address of the expression will be the address of the last lvalue encountered *)
												| M_ADDR ->  []
												(* After dereferencing, the new address of lvalue will become the result of the expression and the deref will return M[R0] *)
												| M_DEREF -> if get_lvalue then [] else ["LDR R0 R0 #0"]
												(* For INC and DEC, get the value of the last lvalue, incr or decr and change its value in memory *)
												| M_PRE_INC -> ["ADD R1 R0 #0";"LDR R0 R1 #0";"ADD R0 R0 #1";"STR R0 R1 #0"]
												| M_PRE_DEC -> ["ADD R1 R0 #0";"LDR R0 R1 #0";"ADD R0 R0 #-1";"STR R0 R1 #0"]
												| M_POST_INC -> ["ADD R1 R0 #0";"LDR R0 R1 #0";"ADD R0 R0 #1";"STR R0 R1 #0";"ADD R0 R0 #-1"]
												| M_POST_DEC -> ["ADD R1 R0 #0";"LDR R0 R1 #0";"ADD R0 R0 #-1";"STR R0 R1 #0";"ADD R0 R0 #1"]
											end
														(* I compute binary operations by storing the result of the first one on the stack and reusing it after computing the second one *)
		| OP2(op, le1, le2) -> let e1 =  (handle_expr le1 tab false) and e2 = (handle_expr le2 tab false) in
													 e1 @ ["STR R0 R6 #0";"ADD R6 R6 #-1"] @ e2 @ ["ADD R6 R6 #1";"LDR R1 R6 #0"] @ begin
													 match op with
													 	| S_ADD -> ["ADD R0 R0 R1"]
													 	| S_SUB -> ["NOT R0 R0";"ADD R0 R0 #1";"ADD R0 R0 R1"]
													 	| S_MUL -> use_mult := true ; ["GLEA R3 FUN_MULT";"JSRR R3"]
													 	| S_DIV -> use_div := true ; ["GLEA R3 FUN_DIV";"JSRR R3"]
													 	| S_MOD -> use_mod := true ; ["GLEA R3 FUN_MOD";"JSRR R3"]
													 end
		| CMP(op, le1, le2) -> let e1 = (handle_expr le1 tab false) and e2 = (handle_expr le2 tab false) in
													 e1 @ ["STR R0 R6 #0";"ADD R6 R6 #-1"] @ e2 @ ["ADD R6 R6 #1";"LDR R1 R6 #0"] @
													 let brtype = match op with
													 										| C_LT -> "zp"
													 										| C_LE -> "p"
													 										| C_EQ -> "np"
													 							in 
													 let e = ["NOT R0 R0";"ADD R0 R0 #1";"ADD R0 R0 R1"] in
													 let ctrue = ["AND R0 R0 #0";"ADD R0 R0 #1"] in 
													 let cfalse = ["AND R0 R0 #0"] in
													 (gen_condition e ctrue cfalse "CMP" brtype)
		| EIF(le1,le2,le3) -> let e = (handle_expr le1 tab false) and c1 = handle_expr le2 tab false and c2 = handle_expr le3 tab false in gen_condition e c1 c2 "EIF" "z"
		| ESEQ(lle) -> List.concat (List.map (fun le -> handle_expr le tab false) lle)
	end
	in

	 let rec handle_block vdl lcl tab r6 = match vdl with
		| [] -> begin match lcl with 
					| [] -> []
					| h::q -> (handle_code h tab r6) @ (handle_block [] q tab r6)
				end
		| h::q -> begin match h with
							(* When encountering a declaration, I allocate memory for it on the stack by increasing R6 and adding it to my symbols table *)
							| CDECL(l,s,t) -> ("ADD R6 R6 #-1") :: handle_block q lcl ( insert_var_tab tab s r6 false ) (r6+1)
							| _ -> []
						end
	and handle_code lc tab r6 = match lc with | (l,c) -> begin match c with
	 			| CBLOCK(vdl, lcl) -> handle_block (List.rev vdl) lcl tab r6
				| CEXPR le -> handle_expr le tab false
				| CIF(le, lc1,lc2) -> let e = (handle_expr le tab false) in let c1 = (handle_code lc1 tab r6) in let c2 = (handle_code lc2 tab r6) in gen_condition e c1 c2 "IF" "z"
				| CWHILE(le, lc) -> let idstring = string_of_int (!flagcount) in
				incr_flag();
				let condjmp = ["BR" ^ (inverse_condition "z") ^ " IGNORE_JMP_WHILE" ^ idstring ; "GLEA R3 ENDWHILE" ^ idstring ; "JMP R3" ; "IGNORE_JMP_WHILE" ^ idstring] in
				let e = handle_expr le tab false in
				let c = handle_code lc tab r6 in
				 ["STARTWHILE" ^ idstring] @ e @ ["ADD R0 R0 #0"] @ condjmp @ c @ ["GLEA R3 STARTWHILE" ^ idstring ; "JMP R3" ; "ENDWHILE" ^ idstring]
				| CRETURN (Some le) -> (handle_expr le tab false) @ ["LDR R7 R5 #1";"LDR R6 R5 #2";"LDR R5 R5 #3";"RET"]
				| CRETURN None ->  ["AND R0 R0 #0";"LDR R7 R5 #1";"LDR R6 R5 #2";"LDR R5 R5 #3";"RET"]
		end
	in


	 let rec handle_val_dec f tab r4 = match f with
	 	| [] -> []
	 	| CDECL(l,s,t)::q -> globvar := s::(!globvar) ; ( handle_val_dec q (insert_var_tab tab s r4 true) (r4 + 1))
	 														(* New environment for the function : adding its arguments to the symbol table *)
	 	| CFUN(l,s,vl,t,lc)::q -> let funtab = (insert_fun_tab tab s vl) in
	 														(* First instructions of every function : pushing R5,R6,R7 and changing the value of R5 *)
	 														let funbase = ["FUN_USER_" ^ s;"STR R5 R6 #0";"ADD R6 R6 #-1";"ADD R1 R6 #1";"STR R1 R6 #0";"ADD R6 R6 #-1";"STR R7 R6 #0";"ADD R6 R6 #-1";"ADD R5 R6 #0"] in
	 														let c = (handle_code lc funtab 0) in
	 														funbase @ c @ (handle_val_dec q funtab r4)
	in


	let codebody = handle_val_dec f [] 0 in

	incr_flag();
																								(* Init R6 value to the address of the stack *)                                     (*Init R5 and R4 *)                  (*Jump to main*)
	let header = [".ORIG #" ^ (string_of_int orig) ; "LD R6 CST" ^ (string_of_int !flagcount)] @ (print_cst_fill !flagcount stack) @ ["ADD R5 R6 #0";"GLEA R4 STATIC_VAR";"GLEA R3 FUN_USER_main";"JMP R3"] in
	(* Intermediate LC3 code *)
	let protoasm = header
		@ ( if !use_mult then print_mult_fun() else [])
		@ ( if !use_div then print_div_fun() else [])
		@ ( if !use_mod then print_mod_fun() else [])
	  @ codebody
	  @ (fill_strings !strings)
	  @ (fill_glob_var !globvar)
	  @ [".END"] in
	(* Second pass : I calculate the line position of every instruction and use this information to replace my pseudo instruction GLEA by real LC3 code*)
	let finalasm = secondpass protoasm in
	finalasm


(* Please, look at  readme.pdf ! *)