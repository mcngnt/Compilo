open Ctable

let rec print_program fmt code = match code with
	| [] -> Format.pp_print_flush fmt ()
	| h::t -> Format.fprintf fmt "%s\n" h ; print_program fmt t