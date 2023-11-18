
open Ctable
open Cast

exception Error of string

let hexstring_of_int a = Printf.sprintf "x%x" a


(* Inserts new variable in the table with its name, address and scope *)
let insert_var_tab tab s d r = match d with | 0 -> SVAR(s,r,d)::tab | _ -> SVAR(s,r,d)::tab


(* Gets the address of the most recent variable whit name s *)
let rec get_addr_tab tab sx = match tab with
	| [] -> raise (Error("Variable undefined"))
	| SVAR(s,r,d)::q when s=sx -> r
	| h::q -> get_addr_tab q sx

let rec get_depth_tab tab sx = match tab with
	| [] -> raise (Error("Variable undefined"))
	| SVAR(s,r,d)::q when s=sx -> d
	| h::q -> get_depth_tab q sx

let rec fill_cst l = match l with
	| [] -> ""
	| h::q -> let sh = string_of_int h in "CST" ^ sh ^ " .FILL #" ^ sh ^"\n" ^ (fill_cst q)

let rec insert_no_double l x = match l with
	| [] -> [x]
	| h::q when h = x -> h::q
	| h::q -> h::(insert_no_double q x)



let check_file f =
  let orig = 0x3000 in
  let stack = 0x8000 in
  let cstdecl = ref [] in
  let ifcount = ref 0 in

	let rec handle_expr le tab d r6 = match le with | (l,e) -> begin match e with
		| VAR s -> let a = get_addr_tab tab s in "LDR R0 R5 #-" ^ (string_of_int a) ^ "\n"
		| CST x -> cstdecl := insert_no_double (!cstdecl) x; "LD R0 CST" ^ (string_of_int x) ^ "\n"
		| SET_VAR(s,le) -> let a = get_addr_tab tab s in (handle_expr le tab d r6) ^ (if (get_depth_tab tab s) == 0 then "STR R0 R4 #" ^ (string_of_int a) ^ "\n" else "STR R0 R5 #-" ^ (string_of_int a) ^ "\n" )
		| OP1(op, le) -> (handle_expr le tab d r6) ^ begin
											match op with
												| M_MINUS -> "NOT R0 R0\nADD R0 R0 #1\n"
												| M_NOT -> "NOT R0 R0\n"
												| _ -> ""
											end
		| OP2(op, le1, le2) -> (handle_expr le1 tab d r6) ^ "STR R0 R6 #0\nADD R6 R6 #-1\n" ^ (handle_expr le2 tab d (r6+1)) ^ "ADD R6 R6 #1\nLDR R1 R6 #0\n" ^ begin
													 match op with
													 	| S_ADD -> "ADD R0 R0 R1\n"
													 	| S_SUB -> "NOT R0 R0\nADD R0 R0 #1\nADD R0 R0 R1\n"
													 	| _ -> ""
													 end
		| _ -> ""
	end
	in

	 let rec handle_block vdl lcl tab d r6 = match vdl with
		| [] -> begin match lcl with 
					| [] -> ""
					| h::q -> (handle_code h tab d r6) ^ (handle_block [] q tab d r6)
				end
		| h::q -> begin match h with
							| CDECL(l,s,t) -> "ADD R6 R6 #-1\n" ^ handle_block q lcl ( insert_var_tab tab s d r6 ) d (r6+1)
							| _ -> ""
						end
	and handle_code lc tab d r6 = match lc with | (l,c) -> begin match c with
	 			| CBLOCK(vdl, lcl) -> handle_block vdl lcl tab (d+1) r6
				| CEXPR le -> handle_expr le tab d r6
				| CIF(le, lc1,lc2) -> let ifnb = string_of_int !ifcount in ifcount := !ifcount + 1; (handle_expr le tab d r6) ^ "BRz ELSE" ^ ifnb ^ "\n" ^ (handle_code lc1 tab d r6) ^ "BR ENDELSE" ^ ifnb ^ "\n" ^ "ELSE" ^ ifnb ^ "\n" ^ (handle_code lc2 tab d r6) ^ "ENDELSE" ^ ifnb ^ "\n"
				| CRETURN (Some le) -> handle_expr le tab d r6
				| _ -> ""
		end
	in


	 let rec handle_val_dec f tab r4 b = match f with
	 	| [] -> ""
	 	| h::t when b -> (handle_val_dec f tab r4 false) ^ "STATIC .BLKW #1\n"
	 	| CDECL(l,s,t)::q -> ( handle_val_dec q (insert_var_tab tab s 0 r4) (r4 + 1) false)
	 	| CFUN(l,s,vl,t,lc)::q -> (handle_code lc tab 0 0) ^ (handle_val_dec q tab r4 false)
	in

	let codebody = handle_val_dec f [] 0 true in

	".ORIG " ^ (hexstring_of_int orig) ^ "\n" ^ "LD R6 STACK\n" ^ "LD R5 STACK\n" ^ "LEA R4 STATIC\n" ^ "ADD R4 R4 #1\n"
	  ^ codebody
	  ^ (fill_cst !cstdecl) ^
	  "STACK .FILL " ^ (hexstring_of_int stack) ^ "\n" ^ ".END"

