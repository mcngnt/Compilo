
(* The type of tokens. *)

type token = 
  | WHILE
  | TILDE_CHR
  | SUB_CHR
  | STRING_LITERAL of (string)
  | STAR_CHR
  | SEMI_CHR
  | RETURN
  | QUES_CHR
  | OR_OP
  | OPEN_PAREN_CHR
  | OPEN_BRACE_CHR
  | OPEN_ANGLE_CHR
  | NULL
  | NE_OP
  | MOD_CHR
  | LE_OP
  | INTEGER
  | INC_OP
  | IF
  | IDENTIFIER of (string)
  | GE_OP
  | FOR
  | EQ_OP
  | EQ_CHR
  | EOF
  | ELSE
  | DIV_CHR
  | DEC_OP
  | CONSTANT of (int)
  | COMMA_CHR
  | COLON_CHR
  | CLOSE_PAREN_CHR
  | CLOSE_BRACE_CHR
  | CLOSE_ANGLE_CHR
  | BANG_CHR
  | AND_OP
  | AMP_CHR
  | ADD_CHR

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Cast.var_declaration list)
