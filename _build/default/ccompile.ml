open Ctable
open Cast

exception Error of string

let hexstring_of_int a = Printf.sprintf "x%x" a

let ignore_specials s = 
	let rec aux l = match l with
		| [] -> ""
		| h::t when h = '\n' -> "\\n" ^ (aux t)
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
		| []-> ""
		| h::q when h = "LVALUE_ADDR" -> (aux q) ^ h ^ " .BLKW #1\n"
		| h::q ->  (aux q) ^ "V_" ^ h ^ " .BLKW #1\n"
	in
	"STATIC_VAR\n" ^ (aux l)

(* Generate declarations for strings constants encountered during compilation *)
let fill_strings l = 
	let rec aux li = match li with 
		| []-> ""
		| (s,label,n)::q ->  (aux q) ^ label ^ " .STRINGZ \"" ^ (ignore_specials s) ^ "\"" ^ " ; " ^ (string_of_int n) ^ "\n"
	in
	"STRINGS\n" ^ (aux l)

(* Creates a CST label as well as a BR to avoid treating the constant as an instruction *)
let print_cst_fill id x = let idstring = string_of_int id in 
	"BR IGNORE_CST" ^ idstring ^  "\nCST" ^ idstring ^ " .FILL #" ^ (string_of_int x) ^ "\nIGNORE_CST" ^ idstring ^ "\n"

(* LC3 code for arithmetic functions *)
let print_mult_fun () = "FUN_MULT\nADD R0 R0 #0\nBRn MULT_CHANGE_SIGN\nBR MULT_INIT\nMULT_CHANGE_SIGN\nNOT R0 R0\nADD R0 R0 #1\nNOT R1 R1\nADD R1 R1 #1\nMULT_INIT\nAND R2 R2 #0\nADD R2 R2 R0\nAND R0 R0 #0\nMULT_LOOP\nADD R0 R0 R1\nADD R2 R2 #-1\nBRz MULT_STOP\nBR MULT_LOOP\nMULT_STOP\nRET\n"
let print_div_fun () = "FUN_DIV\nAND R2 R2 #0 ; Q\nLD R2 DIV_ISNEG ; Set is_neg to 0\nADD R1 R1 #0\nBRn DIV_A_NEG\nBR DIV_A_POS\nDIV_A_NEG\nNOT R1 R1 ; Change A sign when negative\nADD R1 R1 #1\nADD R0 R0 #0\nBRn DIV_AN_BN\nBR DIV_AN_BP\nDIV_A_POS\nADD R0 R0 #0\nBRn DIV_AP_BN\nBR DIV_AP_BP\nDIV_AN_BN\nNOT R0 R0\nADD R0 R0 #1\nBR DIV_POS\nDIV_AN_BP\nBR DIV_NEG\nDIV_AP_BN\nNOT R0 R0\nADD R0 R0 #1\nBR DIV_NEG\nDIV_AP_BP\nBR DIV_POS\nDIV_NEG\nADD R3 R2 #1\nST R3 DIV_ISNEG ; IS_NEG <- 1\nBR DIV_ENDSIGN\nDIV_POS\nBR DIV_ENDSIGN\nDIV_ENDSIGN\n; Compute the sign of the result in DIV_ISNEG\nDIV_LOOP\nNOT R3 R1\nADD R3 R3 #1\nADD R3 R3 R0 ; R3 <- R0 - R1 = B - R\nBRp DIV_END_LOOP\nAND R3 R3 #0\nADD R3 R3 R0\nNOT R3 R3\nADD R3 R3 #1 ; R3 <- -B\nADD R1 R1 R3 ; R <- R - B\nADD R2 R2 #1\nBR DIV_LOOP\nDIV_END_LOOP\nADD R0 R2 #0\nLD R3 DIV_ISNEG\nBRz DIV_END\nNOT R0 R0\nADD R0 R0 #1\nDIV_END\nRET\nDIV_ISNEG .BLKW #1\n"
let print_mod_fun () = "FUN_MOD\nAND R2 R2 #0 ; Q\nADD R1 R1 #0\nBRn MOD_A_NEG\nBR MOD_ENDSIGN\nMOD_A_NEG\nADD R1 R1 R0\nBRn MOD_A_NEG\nMOD_ENDSIGN\n; Compute the sign of the result in MOD_ISNEG\nMOD_LOOP\nNOT R3 R1\nADD R3 R3 #1\nADD R3 R3 R0 ; R3 <- R0 - R1 = B - R\nBRp MOD_END_LOOP\nAND R3 R3 #0\nADD R3 R3 R0\nNOT R3 R3\nADD R3 R3 #1 ; R3 <- -B\nADD R1 R1 R3 ; R <- R - B\nADD R2 R2 #1\nBR MOD_LOOP\nMOD_END_LOOP\nADD R0 R1 #0\nMOD_END\nRET\n"


let rec forget_comments li = match li with
			| [] -> []
			| h::t when h = ";" -> []
			| h::t -> h::(forget_comments t)

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
  let globvar = ref ["LVALUE_ADDR"] in
  let strings = ref [] in
  let stringcount = ref 0 in

  let incr_flag () = flagcount := !flagcount + 1 in
  let incr_string () = stringcount := !stringcount + 1 in

  (* Generates an if e then ctrue else cfalse statement where is treated differently based on the brtype flag *)
  let gen_condition e ctrue cfalse flagname brtype = 
  	incr_flag();	
  	let idstring = string_of_int !flagcount in
  	let condjmp = "BR" ^ (inverse_condition brtype) ^ " IGNORE_JMP" ^ idstring ^ "\n" ^ "GLEA R3 " ^ flagname ^ "_" ^ "ELSE" ^ idstring ^ "\nJMP R3\n" ^ "IGNORE_JMP" ^ idstring ^ "\n" in
  	e ^ "ADD R0 R0 #0\n" ^ condjmp ^ ctrue ^ "GLEA R3 " ^ flagname ^ "_" ^ "ENDELSE" ^ idstring ^ "\nJMP R3\n" ^ flagname ^ "_" ^  "ELSE" ^ idstring ^ "\n" ^ cfalse ^ flagname ^ "_" ^  "ENDELSE" ^ idstring ^ "\n"
  in


  (* Puts the value of a variable in R0 by first storing the address in R1 *)
  (* I store the address in R1 because if I did LDR R0 R4 #offest, I would be limited to 32 offset, 
  limiting the number of static variables to 32 *)
  let get_var a isglob inR0 = 
  	incr_flag();
  	"LD R1 CST" ^ (string_of_int !flagcount) ^ "\n" ^ (if isglob then "ADD R1 R4 R1\nLDR R1 R1 #0 ; Put variable content in R0\n" else "NOT R1 R1\nADD R1 R1 #1\nADD R1 R5 R1\nLDR R1 R1 #0 ; Put variable content in R0\n") ^ (if inR0 then "ADD R0 R1 #0\n" else "") ^ (print_cst_fill !flagcount a)
  in

  (* Sets the value of a variable to the content of R0 by first storing the address in R1 *)
  let set_var a isglob =
  	incr_flag();
  	"LD R1 CST" ^ (string_of_int !flagcount) ^ "\n" ^ (if isglob then "ADD R1 R4 R1\nSTR R0 R1 #0 ; Change variable content to R0\n" else "NOT R1 R1\nADD R1 R1 #1\nADD R1 R5 R1\nSTR R0 R1 #0 ; Change variable content to R0\n") ^ (print_cst_fill !flagcount a)
  in


  (* Puts in LVALUE_ADDR the real address of the variable based on its scope and offset *)
  let set_lvalue addr isglob = 
    incr_flag();
    "LD R0 CST" ^ (string_of_int !flagcount) ^ "\n" ^ ( if isglob then "ADD R0 R0 R4\n" else "NOT R0 R0\nADD R0 R0 #1\nADD R0 R0 R5\n" ) ^ "STR R0 R4 #-1 ; Change lvalue\n" ^ (print_cst_fill !flagcount addr)
  in

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
		let asml = String.split_on_char '\n' asm in

		let rec find_line sx env = match env with
			| [] -> raise (Error("Label " ^ sx ^ " not defined."))
			| (s,n)::t when s = sx -> n
			| h::t -> find_line sx t
		in

		(* Stores every label and string declaration with their line number in the code *)
		let rec create_env l count = match l with
			| [] -> []
			| h::t -> let args = forget_comments (String.split_on_char ' ' h) in
								let nbargs = List.length args in
								if nbargs = 0 then create_env t count else begin
								begin match List.hd args with
									| ".ORIG" -> create_env t count
									| "GLEA" -> create_env t (count + 3)
									| "RET" | "RTI" | "PUTS" | "GETC" | "OUT" | "HALT" -> create_env t (count + 1)
									| _ when nbargs = 1 -> (h,count)::(create_env t count)
									| _ when nbargs > 1 && List.nth args 1 = ".STRINGZ" -> let size = get_string_len (List.hd args) in (List.hd args,count)::(create_env t (count + size + 1))
									| _ -> create_env t (count + 1)
							  end
							 end
			in

		(* Print the line numbers of static variables : DEBUG *)
		let rec print_env env = match env with
			| [] -> ""
			| (s,c)::t when s = "STATIC_VAR" -> "; mem " ^ (string_of_int c) ^ " " ^ (print_env t)
			| (s,c)::t when s = ".END" -> (string_of_int (c-1))
			| (s,c)::t -> (print_env t)
		in

		(* Replaces GLEA, an instruction used for accessing labels and strings more than 256 lines away, with real LC3 code *)
		let rec replace_fun l env = match l with
			| [] -> ""
			| h::t -> let args = String.split_on_char ' ' h in
								begin match List.hd args with
										(* Instruction example -> GLEA RX LABEL where x \in {0,1,2,3} *)
									| "GLEA" -> 
															let label = (List.nth args 2) in
															let r = (List.nth args 1) in
															let n = find_line label env in incr_flag();
															let msg = "LD " ^ r ^ " CST" ^ (string_of_int !flagcount) ^ " ; " ^ r ^ " <- address of label " ^ label ^ "\n" ^ (print_cst_fill !flagcount n) in
															msg ^ (replace_fun t env)
									| _ -> h ^ "\n" ^ (replace_fun t env)
								end
		in
		let env = create_env asml orig in
		(replace_fun asml env) ^ (print_env env)
	in

	let rec gen_call s lle tab n = match lle with
			| [] -> incr_flag(); "GLEA R3 FUN_USER_" ^ s ^ "\nJSRR R3\nLD R1 CST" ^ (string_of_int !flagcount) ^ "\nADD R6 R6 R1 ; Remove " ^ s ^ "'s args from the stack\n" ^ (print_cst_fill !flagcount n)
			| h::t -> let msge = handle_expr h tab in
								msge ^ "STR R0 R6 #0 ; Adding arg on the stack to call " ^ s ^ "\nADD R6 R6 #-1\n" ^ (gen_call s t tab n)

	and handle_expr le tab = match le with | (l,e) -> begin match e with
																																(* Change the current lvalue address to the variable's address and set R0 to its value*)
		| VAR s -> let a,isglob = get_addr_tab tab s in let slvalue = (set_lvalue a isglob) in slvalue ^ (get_var a isglob  true) (* Set LVALUE_VAR to a and isglob *)
		| CST x -> incr_flag() ; "LD R0 CST" ^ (string_of_int !flagcount) ^ " ; R0 <- cst " ^ (string_of_int x) ^ "\n" ^ (print_cst_fill !flagcount x)
		| STRING s ->  incr_string(); strings := (s,"STRING" ^ (string_of_int !stringcount), String.length s)::!strings ; "GLEA R0 STRING" ^ (string_of_int !stringcount) ^ "\n"
		| NULLPTR -> "AND R0 R0 #0\n"
		| SET_VAR(s,le) -> let a,isglob = get_addr_tab tab s in let e = (handle_expr le tab) in e ^ (set_var a isglob )
		| SET_VAL(s,le) -> let a,isglob = get_addr_tab tab s in let e = (handle_expr le tab) in e ^ (get_var a isglob  false) ^  "\nSTR R0 R1 #0 ; R0 <- M[R1]\n"
		| CALL(s,lle) when s = "puts" -> let e = handle_expr (List.hd lle) tab in e ^ "PUTS\n"
		| CALL(s,lle) when s = "putc" -> let e = handle_expr (List.hd lle) tab in e ^ "OUT\n"
		| CALL(s,lle) when s = "getc" -> "GETC\n"
		| CALL(s,lle) -> "; Call function " ^ s ^ "\n" ^ gen_call s lle tab (List.length lle)
		| OP1(op, le) -> let msg = (handle_expr le tab) in
										 msg ^ begin
											match op with
												| M_MINUS -> "NOT R0 R0\nADD R0 R0 #1 ;  R0 <- -R0\n"
												| M_NOT -> "NOT R0 R0\n"
												(* The address of the expression will be the address of the last lvalue encountered *)
												| M_ADDR ->  "LDR R0 R4 #-1 ; Get address of the last lvalue\n"
												(* After dereferencing, the new address of lvalue will become the result of the expression and the deref will return M[R0] *)
												| M_DEREF -> "STR R0 R4 #-1\nLDR R0 R0 #0 ; Deref the last lvalue\n"
												(* For INC and DEC, get the value of the last lvalue, incr or decr and change its value in memory *)
												| M_PRE_INC -> "LDR R1 R4 #-1\nLDR R0 R1 #0\nADD R0 R0 #1\nSTR R0 R1 #0 ; ++(lvalue)\n"
												| M_PRE_DEC -> "LDR R1 R4 #-1\nLDR R0 R1 #0\nADD R0 R0 #-1\nSTR R0 R1 #0 ; --(lvalue)\n"
												| M_POST_INC -> "LDR R1 R4 #-1\nLDR R0 R1 #0\nADD R0 R0 #1\nSTR R0 R1 #0\nADD R0 R0 #-1 ; (lvalue)++\n"
												| M_POST_DEC -> "LDR R1 R4 #-1\nLDR R0 R1 #0\nADD R0 R0 #-1\nSTR R0 R1 #0\nADD R0 R0 #1 ; (lvalue)--\n"
											end
														(* I compute binary operations by storing the result of the first one on the stack and reusing it after computing the second one *)
		| OP2(op, le1, le2) -> let e1 =  (handle_expr le1 tab) and e2 = (handle_expr le2 tab) in
													 "; e1\n" ^ e1 ^ "STR R0 R6 #0 ; Store R0 on the stack\nADD R6 R6 #-1 ; Increase the stack\n; e2\n" ^ e2 ^ "ADD R6 R6 #1 ; Decrease the stack\nLDR R1 R6 #0 ; Retrieve upmost result on the stack in R1\n" ^ begin
													 match op with
													 	| S_ADD -> "ADD R0 R0 R1 ; R0 <- R0 + R1\n"
													 	| S_SUB -> "NOT R0 R0\nADD R0 R0 #1 ;  R0 <- -R0\nADD R0 R0 R1 ; R0 <- R1 - R0\n"
													 	| S_MUL -> "GLEA R3 FUN_MULT\nJSRR R3 ; Multiply R0 and R1\n"
													 	| S_DIV -> "GLEA R3 FUN_DIV\nJSRR R3 ; Divide R1 by R0\n"
													 	| S_MOD -> "GLEA R3 FUN_MOD\nJSRR R3 ; Compute R1 % R0\n"
													 end
		| CMP(op, le1, le2) -> let e1 = (handle_expr le1 tab) and e2 = (handle_expr le2 tab) in
													 e1 ^ "STR R0 R6 #0 ; Store R0 on the stack\nADD R6 R6 #-1 ; Increase the stack\n" ^ e2 ^ "ADD R6 R6 #1 ; Decrease the stack\nLDR R1 R6 #0 ;  Retrieve upmost result on the stack in R1\n" ^
													 let brtype = match op with
													 										| C_LT -> "zp"
													 										| C_LE -> "p"
													 										| C_EQ -> "np"
													 							in 
													 let e = "NOT R0 R0\nADD R0 R0 #1 ;  R0 <- -R0\nADD R0 R0 R1 ; Compute R1 - R0 for comparison\n" in
													 let ctrue = "AND R0 R0 #0\nADD R0 R0 #1 ; If comparison true, set R0 to 1\n" in 
													 let cfalse = "AND R0 R0 #0 ; If comparison false, set R0 to 0\n" in
													 (gen_condition e ctrue cfalse "CMP" brtype) ^ "; R0 <-  e1 " ^ begin match op with
													 																																					 			| C_LT -> ">"
													 																																					 			| C_LE -> "<"
													 																																					 			| C_EQ -> "=="
													 																																					 end ^ " e2\n"
		| EIF(le1,le2,le3) -> let e = (handle_expr le1 tab) and c1 = handle_expr le2 tab and c2 = handle_expr le3 tab in gen_condition e c1 c2 "EIF" "z"
		| ESEQ(lle) -> List.fold_left (fun acc le -> acc ^ (handle_expr le tab)) "" lle
	end
	in

	 let rec handle_block vdl lcl tab r6 = match vdl with
		| [] -> begin match lcl with 
					| [] -> ""
					| h::q -> (handle_code h tab r6) ^ (handle_block [] q tab r6)
				end
		| h::q -> begin match h with
							(* When encountering a declaration, I allocate memory for it on the stack by increasing R6 and adding it to my symbols table *)
							| CDECL(l,s,t) -> "ADD R6 R6 #-1 ; Add variable " ^ s ^ " to the stack \n" ^ handle_block q lcl ( insert_var_tab tab s r6 false ) (r6+1)
							| _ -> ""
						end
	and handle_code lc tab r6 = match lc with | (l,c) -> begin match c with
	 			| CBLOCK(vdl, lcl) -> handle_block (List.rev vdl) lcl tab r6
				| CEXPR le -> handle_expr le tab
				| CIF(le, lc1,lc2) -> let e = (handle_expr le tab) in let c1 = (handle_code lc1 tab r6) in let c2 = (handle_code lc2 tab r6) in gen_condition e c1 c2 "IF" "z"
				| CWHILE(le, lc) -> let idstring = string_of_int (!flagcount) in
				incr_flag();
				let condjmp = "BR" ^ (inverse_condition "z") ^ " IGNORE_JMP_WHILE" ^ idstring ^ "\n" ^ "GLEA R3 ENDWHILE" ^ idstring ^ "\nJMP R3\n" ^ "IGNORE_JMP_WHILE" ^ idstring ^ "\n" in
				let e = handle_expr le tab in
				let c = handle_code lc tab r6 in
				 "STARTWHILE" ^ idstring ^ "\n" ^ e ^ "ADD R0 R0 #0\n" ^ condjmp ^ c ^ "GLEA R3 STARTWHILE" ^ idstring ^ "\nJMP R3\n" ^ "ENDWHILE" ^ idstring ^ "\n"
				| CRETURN (Some le) -> (handle_expr le tab) ^ "LDR R7 R5 #1 ; Restore R7\nLDR R6 R5 #2 ; Restore R6\nLDR R5 R5 #3 ; Restore R5\nRET\n"
				| CRETURN None ->  "AND R0 R0 #0\nLDR R7 R5 #1 ; Restore R7\nLDR R6 R5 #2 ; Restore R6\nLDR R5 R5 #3 ; Restore R5\nRET\n"
		end
	in


	 let rec handle_val_dec f tab r4 = match f with
	 	| [] -> ""
	 	| CDECL(l,s,t)::q -> globvar := s::(!globvar) ; ( handle_val_dec q (insert_var_tab tab s r4 true) (r4 + 1))
	 	| CFUN(l,s,vl,t,lc)::q -> let funtab = (insert_fun_tab tab s vl) in
	 														let funbase = "FUN_USER_" ^ s ^ "\nSTR R5 R6 #0 ; Store R5 on the stack\nADD R6 R6 #-1\nADD R1 R6 #1\nSTR R1 R6 #0 ; Store R6 on the stack\nADD R6 R6 #-1\nSTR R7 R6 #0 ; Store R7 on the stack\nADD R6 R6 #-1\nADD R5 R6 #0 ; R5 <- R6\n" in
	 														let c = (handle_code lc funtab 0) in
	 														funbase ^ c ^ (handle_val_dec q funtab r4)
	in

	let codebody = handle_val_dec f [] 0 in

	incr_flag();
																											(* Init R6 value to the address of the stack *)                      (*Init R5 and R4 and leave space to store LVALUE_ADDR at offset -1*)   (*Jump to main*)
	let header = ".ORIG " ^ (hexstring_of_int orig) ^ "\nLD R6 CST" ^ (string_of_int !flagcount) ^ " ; Init R6 value to the start of the stack\n" ^ (print_cst_fill !flagcount stack) ^ "ADD R5 R6 #0\nGLEA R4 STATIC_VAR\nADD R4 R4 #1\nGLEA R3 FUN_USER_main\nJMP R3\n" in
	let protoasm = header
		^ print_mult_fun()
		^ print_div_fun()
		^ print_mod_fun()
	  ^ codebody
	  ^ (fill_strings !strings)
	  ^ (fill_glob_var !globvar)
	  ^ "\n.END" in
	let finalasm = secondpass protoasm in
	finalasm


(* 
Features :
		- Can handle more than 32 static variables and local variables at the same time (normally difficult beacause the offset is in the -32,32 range)
		- Can jump to arbitrarly far away piece of code with the custom instruction GLEA, converted to real LC3 code in a second pass
		- Division, multiplication and modulo operation implemented in a subroutine to be more lines-efficient
		- Can handle IF and WHILE statements more than 256 lines long by using GLEA and JMP
 *)

 (* 
Possible improvements :
	- Dont calculate lvalue everytime
	- Use naive instruction when small offset
  *)