
			(* R4 * R5 * R6 *  R7 * tabl *)

         (* name * address * isGlobal             name * args*)
type symb = SVAR of string * int * bool | SFUN of string * string list

(* 				(* opname     p1       p2        p3      commentary   *)
type op = INSTR of string * string * string * string * string
		 | 
		 | LABEL of string *)