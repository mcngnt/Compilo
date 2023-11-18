
(* open Cast *)


type ttype = TTINT | TTPTR of ttype | TTNULL

type vdeclt = VART of string * ttype * int | FUNT of string * (ttype list) * ttype








(* type tloc_expr = location * (ttype * expr)


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
  | TCRETURN of tloc_expr option (** return; ou return (e); *) *)