
open Ctable
open Cast

exception Error of string

(* Problems : -cst label
							-> add to tab
							-> using nearby to tp to static
					 -br label
					 	-> use nearby + LDI *)

let hexstring_of_int a = Printf.sprintf "x%x" a


(* Inserts new variable in the table with its name, address and scope *)
let insert_var_tab tab s r isglob = match isglob with | true -> SVAR(s,r)::tab | false -> SVAR(s,-r)::tab



(* Gets the address of the most recent variable whit name s *)
let rec get_addr_tab tab sx = match tab with
	| [] -> raise (Error("Variable undefined"))
	| SVAR(s,r)::q when s=sx -> r
	| h::q -> get_addr_tab q sx



let cst_string_of_int x = match x with
	| _ when x >= 0 -> string_of_int x
	| _ -> "M" ^ (string_of_int (-x))

let rec fill_glob_var l = match l with
	| []-> ""
	| h::q when h = "STATIC" ->  (fill_glob_var q) ^ h ^ " .BLKW #1\n" 
	| h::q ->  (fill_glob_var q) ^ "V_" ^ h ^ " .BLKW #1\n" 

let rec insert_no_double l x = match l with
	| [] -> [x]
	| h::q when h = x -> h::q
	| h::q -> h::(insert_no_double q x)


let print_cst_fill id x = let idstring = string_of_int id in 
	"BR IGNORE_CST" ^ idstring ^  "\nCST" ^ idstring ^ " .FILL #" ^ (string_of_int x) ^ "\nIGNORE_CST" ^ idstring ^ "\n"


let gen_condition e ctrue cfalse id flagname brtype = let idstring = string_of_int id in
	e ^ "BR" ^ brtype ^ " " ^ flagname ^ "_" ^ "ELSE" ^ idstring ^ "\n" ^ ctrue ^ "BR " ^ flagname ^ "_" ^ "ENDELSE" ^ idstring ^ "\n" ^ flagname ^ "_" ^  "ELSE" ^ idstring ^ "\n" ^ cfalse ^ flagname ^ "_" ^  "ENDELSE" ^ idstring ^ "\n"

let negate_r0 () = "NOT R0 R0\nADD R0 R0 #1\n"

let print_test_fun () = "FUN_TEST\nLD R0 MCST\nRET\nMCST .FILL #666\n"




let check_file f =
  let orig = 0x3000 in
  let stack = 0xFDFF in
  let flagcount = ref 0 in
  let globvar = ref ["STATIC"] in
  let lastaddr = ref 0 in

  let incr_flag () = flagcount := !flagcount + 1 in



  let fun_accessing asm = 
		let asml = String.split_on_char '\n' asm in

		let rec find_pos sx env = match env with
			| [] -> raise (Error("Function " ^ sx ^ " not defined."))
			| (s,n)::t when s = sx -> n
			| h::t -> find_pos sx t
		in

		let rec create_env l count = match l with
			| [] -> []
			| h::t -> begin  match List.hd (String.split_on_char '_' (List.hd (String.split_on_char ' ' h))) with
									| "FUN" -> (h,count)::(create_env t count)
									| "GOTO" -> create_env t (count + 3)
									| "RET" | "RTI" -> create_env t (count + 1)
									| _ when List.length (String.split_on_char ' ' h) = 1 -> create_env t count
									| _ -> create_env t (count + 1)
							  end
			in

		let rec replace_fun l env = match l with
			| [] -> ""
			| h::t -> begin match List.hd (String.split_on_char ' ' h) with
									| "GOTO" -> let n = find_pos (List.nth (String.split_on_char ' ' h) 1) env in incr_flag(); let msg = "LD R3 CST" ^ (string_of_int !flagcount) ^ "\nJSRR R3\n" ^ (print_cst_fill !flagcount n) in msg ^ (replace_fun t env)
									| _ -> h ^ "\n" ^ (replace_fun t env)
								end
		in
		let env = create_env asml orig in
		replace_fun asml env
	in





	let rec handle_expr le tab r6 = match le with | (l,e) -> begin match e with
		| VAR s -> incr_flag(); let a = get_addr_tab tab s in let msg =  "LD R0 CST" ^ (string_of_int !flagcount) ^ "\n" ^ (if a >= 0 then "ADD R0 R0 R4\n" else "ADD R0 R0 R5\n" ) ^ "LDR R0 R0 #0\n" ^ (print_cst_fill !flagcount a) in lastaddr := a ; msg
		| CST x -> incr_flag() ; "LD R0 CST" ^ (string_of_int !flagcount) ^ "\n" ^ (print_cst_fill !flagcount x)
		| SET_VAR(s,le) -> let a = get_addr_tab tab s in (handle_expr le tab r6) ^ (if a >= 0 then "STR R0 R4 #" ^ (string_of_int a) ^ "\n" else "STR R0 R5 #" ^ (string_of_int a) ^ "\n" )
		| SET_VAL(s,le) -> let a = get_addr_tab tab s in (handle_expr le tab r6) ^ (if a >= 0 then "LDR R0 R4 #" ^ (string_of_int a) ^ "\n" ^ "ADD R1 R0 R4\nSTR R0 R1 #0\n" else "LDR R0 R5 #" ^ (string_of_int a) ^ "\n" ^ "ADD R1 R0 R5\nSTR R0 R1 #0\n" )
		| OP1(op, le) -> let msg = (handle_expr le tab r6) in msg ^ begin
											match op with
												| M_MINUS -> negate_r0()
												| M_NOT -> "NOT R0 R0\n"
												| M_ADDR ->  incr_flag(); "LD R0 CST" ^ (string_of_int !flagcount) ^ "\n" ^ (print_cst_fill !flagcount !lastaddr)
												| M_DEREF -> incr_flag(); (gen_condition "" "ADD R0 R0 R5\n" "ADD R0 R0 R4\n" !flagcount "DEREF" "zp") ^ "LDR R0 R0 #0\n"
												| _ -> ""
											end
		| OP2(op, le1, le2) -> (handle_expr le1 tab r6) ^ "STR R0 R6 #0\nADD R6 R6 #-1\n" ^ (handle_expr le2 tab (r6+1)) ^ "ADD R6 R6 #1\nLDR R1 R6 #0\n" ^ begin
													 match op with
													 	| S_ADD -> "ADD R0 R0 R1\n"
													 	| S_SUB -> (negate_r0()) ^ "ADD R0 R0 R1\n"
													 	| _ -> ""
													 end
		| CMP(op, le1, le2) -> (handle_expr le1 tab r6) ^ "STR R0 R6 #0\nADD R6 R6 #-1\n" ^ (handle_expr le2 tab (r6+1)) ^ "ADD R6 R6 #1\nLDR R1 R6 #0\n" ^
													 let brtype = match op with
													 										| C_LT -> "zp"
													 										| C_LE -> "p"
													 										| C_EQ -> "np"
													 							in 
													 let e = (negate_r0()) ^ "ADD R0 R0 R1\n" in let ctrue = "AND R0 R0 #0\nADD R0 R0 #1\n" in let cfalse = "AND R0 R0 #0\n" in incr_flag(); gen_condition e ctrue cfalse !flagcount "CMP" brtype
		| _ -> ""
	end
	in

	 let rec handle_block vdl lcl tab r6 = match vdl with
		| [] -> begin match lcl with 
					| [] -> ""
					| h::q -> (handle_code h tab r6) ^ (handle_block [] q tab r6)
				end
		| h::q -> begin match h with
							| CDECL(l,s,t) -> "ADD R6 R6 #-1\n" ^ handle_block q lcl ( insert_var_tab tab s r6 false ) (r6+1)
							| _ -> ""
						end
	and handle_code lc tab r6 = match lc with | (l,c) -> begin match c with
	 			| CBLOCK(vdl, lcl) -> handle_block vdl lcl tab r6
				| CEXPR le -> handle_expr le tab r6
				| CIF(le, lc1,lc2) -> incr_flag(); let e = (handle_expr le tab r6) in let c1 = (handle_code lc1 tab r6) in let c2 = (handle_code lc2 tab r6) in gen_condition e c1 c2 !flagcount "IF" "z"
				| CWHILE(le, lc) -> let flagstring = string_of_int (!flagcount) in incr_flag(); "STARTWHILE" ^ flagstring ^ "\n" ^ (handle_expr le tab r6) ^ "BRz ENDWHILE" ^ flagstring ^ "\n" ^ (handle_code lc tab r6) ^ "BR STARTWHILE" ^ flagstring ^ "\n" ^ "ENDWHILE" ^ flagstring ^ "\n"
				| CRETURN (Some le) -> handle_expr le tab r6
				| _ -> ""
		end
	in


	 let rec handle_val_dec f tab r4 = match f with
	 	| [] -> ""
	 	| CDECL(l,s,t)::q -> globvar := s::(!globvar) ; ( handle_val_dec q (insert_var_tab tab s r4 true) (r4 + 1))
	 	| CFUN(l,s,vl,t,lc)::q -> (handle_code lc tab 1) ^ (handle_val_dec q tab r4)
	in

	let codebody = handle_val_dec f [] 0 in

	let rawasm = ".ORIG " ^ (hexstring_of_int orig) ^ "\nLD R6 STACK\nLD R5 STACK\nLEA R4 STATIC\nADD R4 R4 #1\n"
		^ "GOTO FUN_MAIN\n"
		(* ^ print_test_fun() *)
		^ "FUN_MAIN\n"
	  ^ codebody
	  (* ^ "GOTO FUN_TEST\n" *)
	  ^ (fill_glob_var !globvar) ^
	  "STACK .FILL " ^ (hexstring_of_int stack) ^ "\n" ^ ".END" in
	  (* rawasm *)
	let funasm = fun_accessing rawasm in
	funasm

