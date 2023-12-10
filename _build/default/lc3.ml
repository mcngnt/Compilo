open Ctable


let instr_format s = 
	let args = String.split_on_char ' ' s in
	match List.hd args with
		| "LD" | "LDR" | "ST" | "STR" | "ADD" |"AND" | "NOT" ->
		(List.hd args) ^ " " ^ (List.nth args 1) ^ "," ^ (List.nth args 2) ^ (if List.length args > 3 then "," ^ (List.nth args 3) else "")
		| _ -> s

let rec print_program fmt code = match code with
	| [] -> Format.pp_print_flush fmt ()
	| h::t -> Format.fprintf fmt "%s\n" (instr_format h) ; print_program fmt t