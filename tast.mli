open Cast


(* Custom types are utilized to check the typing during the AST traversal. 
I included the NULL pointer type because the NULL expression can be used with any PTR type. *)
type ttype = TTINT | TTPTR of ttype | TTNULL

(* Type used to describe the elements of the environment. *)
                    (* name  * type   * depth         name   * args_type    * return type *)
type vdeclt = VART of string * ttype * int | FUNT of string * (ttype list) * ttype

(* Here, depth represents the number of nested blocks where the variable was encountered. For instance, a global variable will have a depth of 0, while an argument of a function will have a depth of 1. *)



type tloc_expr = location * texpr * ttype
and texpr =

  | TVAR of string (** une variable --- toujours de type int. *)
  | TCST of int (** une constante entiere. *)
  | TSTRING of string (** une constante chaine. *)
  | TNULLPTR (* the null pointer *)
  | TSET_VAR of string * tloc_expr (** affectation x=e. *)
  | TSET_VAL of string * tloc_expr (** affectation *x=e. *)
  | TCALL of string * tloc_expr list (** appel de fonction f(e1,...,en) *)
  
  | TOP1 of mon_op * tloc_expr
    (** OP1(mop, e) dénote -e, ~e, e++, e--, ++e, --e, &e ou *e. *)
  | TOP2 of bin_op * tloc_expr * tloc_expr
    (** OP2(bop,e,e') dénote e*e', e/e', e%e',
                             e+e', e-e'. *)
  | TCMP of cmp_op * tloc_expr * tloc_expr
    (** CMP(cop,e,e') vaut e<e', e<=e', ou e==e' *)
  | TEIF of tloc_expr * tloc_expr * tloc_expr
    (** EIF(e1,e2,e3) est e1?e2:e3 *)
  | TESEQ of tloc_expr list
    (** e1, ..., en [sequence, analogue a e1;e2 au niveau code];
      si n=0, represente skip. *)

type tvar_declaration =
  | TCDECL of location * string * ctyp
    (** declaration de variable de type ctyp. *)
  | TCFUN of location * string * tvar_declaration list * ctyp * tloc_code
    (** fonction avec ses arguments, le type du resultat et son code. *)
and tloc_code = location * tcode
and tcode =
    TCBLOCK of tvar_declaration list * tloc_code list (** { declarations; code; } *)
  | TCEXPR of tloc_expr (** une expression e; vue comme instruction. *)
  | TCIF of tloc_expr * tloc_code * tloc_code (** if (e) c1; else c2; *)
  | TCWHILE of tloc_expr * tloc_code (** while (e) c1; *)
  | TCRETURN of tloc_expr option (** return; ou return (e); *)