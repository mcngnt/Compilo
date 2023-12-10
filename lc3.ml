open Ctable

let convert_code code = 
	code

let print_program fmt code = 
	let msg = convert_code code in
	Format.fprintf fmt "%s" msg;
	Format.pp_print_flush fmt ()
