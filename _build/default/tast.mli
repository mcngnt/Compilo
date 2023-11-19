(* Custom types are utilized to check the typing during the AST traversal. 
I included the NULL pointer type because the NULL expression can be used with any PTR type. *)
type ttype = TTINT | TTPTR of ttype | TTNULL

(* Type used to describe the elements of the environment. *)
                    (* name  * type   * depth         name   * args_type    * return type *)
type vdeclt = VART of string * ttype * int | FUNT of string * (ttype list) * ttype

(* Here, depth represents the number of nested blocks where the variable was encountered. For instance, a global variable will have a depth of 0, while an argument of a function will have a depth of 1. *)




