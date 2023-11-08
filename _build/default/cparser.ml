
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHILE
    | TILDE_CHR
    | SUB_CHR
    | STRING_LITERAL of (
# 45 "cparser.mly"
       (string)
# 18 "cparser.ml"
  )
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
    | IDENTIFIER of (
# 43 "cparser.mly"
       (string)
# 38 "cparser.ml"
  )
    | GE_OP
    | FOR
    | EQ_OP
    | EQ_CHR
    | EOF
    | ELSE
    | DIV_CHR
    | DEC_OP
    | CONSTANT of (
# 44 "cparser.mly"
       (int)
# 51 "cparser.ml"
  )
    | COMMA_CHR
    | COLON_CHR
    | CLOSE_PAREN_CHR
    | CLOSE_BRACE_CHR
    | CLOSE_ANGLE_CHR
    | BANG_CHR
    | AND_OP
    | AMP_CHR
    | ADD_CHR
  
end

include MenhirBasics

# 1 "cparser.mly"
  

(*
 *	Copyright (C) 2023 by Laboratoire Méthodes Formelles (LMF),
 *      UMR 9021  Université Paris-Saclay, CNRS et ENS Paris-Saclay.
 *      Modified by Mihaela Sighireanu.
 *
 *      Copyright (C) 2005, 2006 by Laboratoire Spécification et Vérification (LSV),
 *      UMR 8643 CNRS & ENS Cachan.
 *      Written by Jean Goubault-Larrecq.  Derived from the csur project.
 *
 *      Permission is granted to anyone to use this software for any
 *      purpose on any computer system, and to redistribute it freely,
 *      subject to the following restrictions:
 *
 *      1. Neither the author nor its employer is responsible for the consequences of use of
 *              this software, no matter how awful, even if they arise
 *              from defects in it.
 *
 *      2. The origin of this software must not be misrepresented, either
 *              by explicit claim or by omission.
 *
 *      3. Altered versions must be plainly marked as such, and must not
 *              be misrepresented as being the original software.
 *
 *      4. This software is restricted to non-commercial use only.  Commercial
 *              use is subject to a specific license, obtainable from LMF.
 *
*)

(* Analyse syntaxique d'un sous-ensemble (tres) reduit de C.
 *)

open Cast

exception Parse_error of Cast.location * string
let sup_locator loc1 loc2 = 
  let st1, end1 = loc1 in
  let st2, end2 = loc2 in st1, end2


# 109 "cparser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_file) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: file. *)

  | MenhirState003 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 003.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState007 : ((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_state
    (** State 007.
        Stack shape : type_specifier identifier.
        Start symbol: file. *)

  | MenhirState009 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 009.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState015 : (((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_parameter_list, _menhir_box_file) _menhir_state
    (** State 015.
        Stack shape : type_specifier identifier parameter_list.
        Start symbol: file. *)

  | MenhirState022 : (('s, _menhir_box_file) _menhir_cell1_function_declarator, _menhir_box_file) _menhir_state
    (** State 022.
        Stack shape : function_declarator.
        Start symbol: file. *)

  | MenhirState025 : (('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_state
    (** State 025.
        Stack shape : open_block.
        Start symbol: file. *)

  | MenhirState029 : (('s, _menhir_box_file) _menhir_cell1_STRING_LITERAL, _menhir_box_file) _menhir_state
    (** State 029.
        Stack shape : STRING_LITERAL.
        Start symbol: file. *)

  | MenhirState034 : (('s, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR, _menhir_box_file) _menhir_state
    (** State 034.
        Stack shape : OPEN_PAREN_CHR.
        Start symbol: file. *)

  | MenhirState042 : (('s, _menhir_box_file) _menhir_cell1_unary_operator, _menhir_box_file) _menhir_state
    (** State 042.
        Stack shape : unary_operator.
        Start symbol: file. *)

  | MenhirState049 : (('s, _menhir_box_file) _menhir_cell1_postfix_expression, _menhir_box_file) _menhir_state
    (** State 049.
        Stack shape : postfix_expression.
        Start symbol: file. *)

  | MenhirState052 : (('s, _menhir_box_file) _menhir_cell1_inc_op, _menhir_box_file) _menhir_state
    (** State 052.
        Stack shape : inc_op.
        Start symbol: file. *)

  | MenhirState055 : (('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_state
    (** State 055.
        Stack shape : identifier.
        Start symbol: file. *)

  | MenhirState058 : (('s, _menhir_box_file) _menhir_cell1_unary_expression, _menhir_box_file) _menhir_state
    (** State 058.
        Stack shape : unary_expression.
        Start symbol: file. *)

  | MenhirState061 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 061.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState064 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 064.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState065 : (('s, _menhir_box_file) _menhir_cell1_dec_op, _menhir_box_file) _menhir_state
    (** State 065.
        Stack shape : dec_op.
        Start symbol: file. *)

  | MenhirState072 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 072.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState074 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 074.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState078 : (('s, _menhir_box_file) _menhir_cell1_additive_expression, _menhir_box_file) _menhir_state
    (** State 078.
        Stack shape : additive_expression.
        Start symbol: file. *)

  | MenhirState080 : (('s, _menhir_box_file) _menhir_cell1_additive_expression, _menhir_box_file) _menhir_state
    (** State 080.
        Stack shape : additive_expression.
        Start symbol: file. *)

  | MenhirState082 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 082.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState084 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 084.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState086 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 086.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState089 : (('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_state
    (** State 089.
        Stack shape : logical_or_expression.
        Start symbol: file. *)

  | MenhirState091 : (('s, _menhir_box_file) _menhir_cell1_logical_and_expression, _menhir_box_file) _menhir_state
    (** State 091.
        Stack shape : logical_and_expression.
        Start symbol: file. *)

  | MenhirState095 : (('s, _menhir_box_file) _menhir_cell1_equality_expression, _menhir_box_file) _menhir_state
    (** State 095.
        Stack shape : equality_expression.
        Start symbol: file. *)

  | MenhirState097 : (('s, _menhir_box_file) _menhir_cell1_equality_expression, _menhir_box_file) _menhir_state
    (** State 097.
        Stack shape : equality_expression.
        Start symbol: file. *)

  | MenhirState101 : ((('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 101.
        Stack shape : logical_or_expression expression.
        Start symbol: file. *)

  | MenhirState102 : ((('s, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR, _menhir_box_file) _menhir_state
    (** State 102.
        Stack shape : expression COMMA_CHR.
        Start symbol: file. *)

  | MenhirState105 : (((('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR, _menhir_box_file) _menhir_state
    (** State 105.
        Stack shape : logical_or_expression expression COLON_CHR.
        Start symbol: file. *)

  | MenhirState108 : (('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_state
    (** State 108.
        Stack shape : logical_or_expression.
        Start symbol: file. *)

  | MenhirState113 : ((('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_state
    (** State 113.
        Stack shape : identifier argument_expression_list.
        Start symbol: file. *)

  | MenhirState114 : (((('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR, _menhir_box_file) _menhir_state
    (** State 114.
        Stack shape : identifier argument_expression_list COMMA_CHR.
        Start symbol: file. *)

  | MenhirState118 : ((('s, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 118.
        Stack shape : OPEN_PAREN_CHR expression.
        Start symbol: file. *)

  | MenhirState124 : (('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_state
    (** State 124.
        Stack shape : whilekw.
        Start symbol: file. *)

  | MenhirState125 : ((('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 125.
        Stack shape : whilekw expression.
        Start symbol: file. *)

  | MenhirState126 : (((('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 126.
        Stack shape : whilekw expression close_paren.
        Start symbol: file. *)

  | MenhirState130 : (('s, _menhir_box_file) _menhir_cell1_return, _menhir_box_file) _menhir_state
    (** State 130.
        Stack shape : return.
        Start symbol: file. *)

  | MenhirState132 : ((('s, _menhir_box_file) _menhir_cell1_return, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 132.
        Stack shape : return expression.
        Start symbol: file. *)

  | MenhirState137 : (('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_state
    (** State 137.
        Stack shape : ifkw.
        Start symbol: file. *)

  | MenhirState138 : ((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 138.
        Stack shape : ifkw expression.
        Start symbol: file. *)

  | MenhirState139 : (((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_state
    (** State 139.
        Stack shape : ifkw expression CLOSE_PAREN_CHR.
        Start symbol: file. *)

  | MenhirState142 : (('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_state
    (** State 142.
        Stack shape : forkw.
        Start symbol: file. *)

  | MenhirState143 : ((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_state
    (** State 143.
        Stack shape : forkw expression_statement.
        Start symbol: file. *)

  | MenhirState144 : (((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_state
    (** State 144.
        Stack shape : forkw expression_statement expression_statement.
        Start symbol: file. *)

  | MenhirState145 : ((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 145.
        Stack shape : forkw expression_statement expression_statement expression.
        Start symbol: file. *)

  | MenhirState146 : (((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 146.
        Stack shape : forkw expression_statement expression_statement expression close_paren.
        Start symbol: file. *)

  | MenhirState149 : (('s, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 149.
        Stack shape : expression.
        Start symbol: file. *)

  | MenhirState152 : ((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 152.
        Stack shape : forkw expression_statement expression_statement close_paren.
        Start symbol: file. *)

  | MenhirState155 : ((((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement, _menhir_box_file) _menhir_state
    (** State 155.
        Stack shape : ifkw expression CLOSE_PAREN_CHR compound_statement.
        Start symbol: file. *)

  | MenhirState158 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 158.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState159 : ((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list, _menhir_box_file) _menhir_state
    (** State 159.
        Stack shape : open_block statement_list.
        Start symbol: file. *)

  | MenhirState164 : ((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_state
    (** State 164.
        Stack shape : open_block declaration_list.
        Start symbol: file. *)

  | MenhirState165 : (((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list, _menhir_box_file) _menhir_state
    (** State 165.
        Stack shape : open_block declaration_list statement_list.
        Start symbol: file. *)

  | MenhirState173 : (('s, _menhir_box_file) _menhir_cell1_external_declaration, _menhir_box_file) _menhir_state
    (** State 173.
        Stack shape : external_declaration.
        Start symbol: file. *)


and ('s, 'r) _menhir_cell1_additive_expression = 
  | MenhirCell1_additive_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_argument_expression_list = 
  | MenhirCell1_argument_expression_list of 's * ('s, 'r) _menhir_state * (Cast.loc_expr list)

and ('s, 'r) _menhir_cell1_close_paren = 
  | MenhirCell1_close_paren of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_compound_statement = 
  | MenhirCell1_compound_statement of 's * ('s, 'r) _menhir_state * (Cast.loc_code)

and ('s, 'r) _menhir_cell1_dec_op = 
  | MenhirCell1_dec_op of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_declaration_list = 
  | MenhirCell1_declaration_list of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_equality_expression = 
  | MenhirCell1_equality_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_expression_statement = 
  | MenhirCell1_expression_statement of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_external_declaration = 
  | MenhirCell1_external_declaration of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_forkw = 
  | MenhirCell1_forkw of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_function_declarator = 
  | MenhirCell1_function_declarator of 's * ('s, 'r) _menhir_state * ((Cast.location * string) * Cast.var_declaration list * Cast.ctyp)

and ('s, 'r) _menhir_cell1_identifier = 
  | MenhirCell1_identifier of 's * ('s, 'r) _menhir_state * (Cast.location * string)

and ('s, 'r) _menhir_cell1_ifkw = 
  | MenhirCell1_ifkw of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_inc_op = 
  | MenhirCell1_inc_op of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_logical_and_expression = 
  | MenhirCell1_logical_and_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_logical_or_expression = 
  | MenhirCell1_logical_or_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_multiplicative_expression = 
  | MenhirCell1_multiplicative_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_open_block = 
  | MenhirCell1_open_block of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_parameter_list = 
  | MenhirCell1_parameter_list of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_postfix_expression = 
  | MenhirCell1_postfix_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_relational_expression = 
  | MenhirCell1_relational_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_return = 
  | MenhirCell1_return of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_statement_list = 
  | MenhirCell1_statement_list of 's * ('s, 'r) _menhir_state * (Cast.loc_code list)

and ('s, 'r) _menhir_cell1_type_specifier = 
  | MenhirCell1_type_specifier of 's * ('s, 'r) _menhir_state * (Cast.ctyp)

and ('s, 'r) _menhir_cell1_unary_expression = 
  | MenhirCell1_unary_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_unary_operator = 
  | MenhirCell1_unary_operator of 's * ('s, 'r) _menhir_state * (Cast.location * token)

and ('s, 'r) _menhir_cell1_whilekw = 
  | MenhirCell1_whilekw of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_CLOSE_PAREN_CHR = 
  | MenhirCell1_CLOSE_PAREN_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COLON_CHR = 
  | MenhirCell1_COLON_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COMMA_CHR = 
  | MenhirCell1_COMMA_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OPEN_PAREN_CHR = 
  | MenhirCell1_OPEN_PAREN_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STRING_LITERAL = 
  | MenhirCell1_STRING_LITERAL of 's * ('s, 'r) _menhir_state * (
# 45 "cparser.mly"
       (string)
# 493 "cparser.ml"
)

and _menhir_box_file = 
  | MenhirBox_file of (Cast.var_declaration list) [@@unboxed]

let _menhir_action_001 =
  fun () ->
    (
# 145 "cparser.mly"
                        ( getloc (), ADD_CHR   )
# 504 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_002 =
  fun _1 ->
    (
# 176 "cparser.mly"
            ( _1 )
# 512 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_003 =
  fun _1 _3 ->
    (
# 178 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_ADD, _1, _3)
	)
# 522 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_004 =
  fun _1 _3 ->
    (
# 182 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_SUB, _1, _3)
	)
# 532 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_005 =
  fun () ->
    (
# 150 "cparser.mly"
                        ( getloc (), AMP_CHR   )
# 540 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_006 =
  fun _1 ->
    (
# 227 "cparser.mly"
                              ( _1 )
# 548 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_007 =
  fun _1 ->
    (
# 110 "cparser.mly"
                                ( [_1] )
# 556 "cparser.ml"
     : (Cast.loc_expr list))

let _menhir_action_008 =
  fun _1 _3 ->
    (
# 111 "cparser.mly"
                                                                   ( 
          _3 :: _1 )
# 565 "cparser.ml"
     : (Cast.loc_expr list))

let _menhir_action_009 =
  fun _1 ->
    (
# 264 "cparser.mly"
                                 ( _1 )
# 573 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_010 =
  fun _1 _3 ->
    (
# 266 "cparser.mly"
     (
	     let locvar, left = _1 in
	     let loc = sup_locator locvar (loc_of_expr _3) in
	     match left with
	       VAR x -> loc, SET_VAR (x, _3)
	     | OP1 (M_DEREF, (_, VAR x)) -> loc, SET_VAL (x, _3)
	     | _ ->
		 raise (Parse_error (loc,
		     "Can only write assignments of the form x=e or *x=e.\n"))
	   )
# 590 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_011 =
  fun () ->
    (
# 147 "cparser.mly"
                        ( getloc (), BANG_CHR  )
# 598 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_012 =
  fun _1 ->
    (
# 155 "cparser.mly"
                           ( _1 )
# 606 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_013 =
  fun _1 ->
    (
# 314 "cparser.mly"
                          ( _1 )
# 614 "cparser.ml"
     : (Cast.location))

let _menhir_action_014 =
  fun () ->
    (
# 75 "cparser.mly"
                              ( getloc () )
# 622 "cparser.ml"
     : (Cast.location))

let _menhir_action_015 =
  fun () ->
    (
# 152 "cparser.mly"
                              ( getloc () )
# 630 "cparser.ml"
     : (Cast.location))

let _menhir_action_016 =
  fun _1 _2 ->
    (
# 318 "cparser.mly"
        ( sup_locator _1 _2, CBLOCK ([], []) )
# 638 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_017 =
  fun _1 _2 _3 ->
    (
# 320 "cparser.mly"
 ( sup_locator _1 _3, CBLOCK ([], List.rev _2) )
# 646 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_018 =
  fun _1 _2 _3 ->
    (
# 322 "cparser.mly"
 ( sup_locator _1 _3, CBLOCK (_2, []) )
# 654 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_019 =
  fun _1 _2 _3 _4 ->
    (
# 324 "cparser.mly"
 ( sup_locator _1 _4, CBLOCK (_2, List.rev _3) )
# 662 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_020 =
  fun _1 ->
    (
# 255 "cparser.mly"
                                ( _1 )
# 670 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_021 =
  fun _1 _3 _5 ->
    (
# 257 "cparser.mly"
 ( 
	  sup_locator (loc_of_expr _1) (loc_of_expr _5),
	  EIF (_1, _3, _5)
	)
# 681 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_022 =
  fun _1 ->
    (
# 71 "cparser.mly"
                    ( getloc (), _1 )
# 689 "cparser.ml"
     : (Cast.location * int))

let _menhir_action_023 =
  fun () ->
    (
# 87 "cparser.mly"
                ( getloc () )
# 697 "cparser.ml"
     : (Cast.location))

let _menhir_action_024 =
  fun _1 _2 ->
    (
# 289 "cparser.mly"
        ( let loc,var = _2 in [CDECL(loc,var,_1)] )
# 705 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_025 =
  fun _1 ->
    (
# 330 "cparser.mly"
          ( _1 )
# 713 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_026 =
  fun _1 _2 ->
    (
# 332 "cparser.mly"
          ( _1 @ _2 )
# 721 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_027 =
  fun _1 ->
    (
# 293 "cparser.mly"
                     ( _1 )
# 729 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_028 =
  fun _1 ->
    (
# 212 "cparser.mly"
                                ( _1 )
# 737 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_029 =
  fun _1 _3 ->
    (
# 214 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_EQ, _1, _3)
	)
# 747 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_030 =
  fun _1 _3 ->
    (
# 218 "cparser.mly"
 ( 
          let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF ((loc, CMP (C_EQ, _1, _3)),
		    (loc, CST 0),
		    (loc, CST 1))
	)
# 760 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_031 =
  fun _1 ->
    (
# 231 "cparser.mly"
                         ( _1 )
# 768 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_032 =
  fun _1 ->
    (
# 279 "cparser.mly"
                                ( _1 )
# 776 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_033 =
  fun _1 _3 ->
    (
# 281 "cparser.mly"
 ( 
	  sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  ESEQ [_1; _3]
	)
# 787 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_034 =
  fun _1 ->
    (
# 345 "cparser.mly"
            ( _1, ESEQ [] )
# 795 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_035 =
  fun _1 ->
    (
# 347 "cparser.mly"
            ( _1 )
# 803 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_036 =
  fun _1 ->
    (
# 415 "cparser.mly"
            ( [_1] )
# 811 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_037 =
  fun _1 ->
    (
# 417 "cparser.mly"
            ( _1 )
# 819 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_038 =
  fun () ->
    (
# 408 "cparser.mly"
          ( [] )
# 827 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_039 =
  fun _1 _2 ->
    (
# 410 "cparser.mly"
          ( _1 @ _2 )
# 835 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_040 =
  fun () ->
    (
# 371 "cparser.mly"
            ( getloc () )
# 843 "cparser.ml"
     : (Cast.location))

let _menhir_action_041 =
  fun _1 _2 _3 ->
    (
# 441 "cparser.mly"
 ( _2, _3, _1 )
# 851 "cparser.ml"
     : ((Cast.location * string) * Cast.var_declaration list * Cast.ctyp))

let _menhir_action_042 =
  fun _1 _2 ->
    (
# 446 "cparser.mly"
 ( 
          let (loc, var), decls, rty = _1 in
	  CFUN (loc, var, decls, rty, _2)
	)
# 862 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_043 =
  fun _1 ->
    (
# 73 "cparser.mly"
                              ( getloc (), _1 )
# 870 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_044 =
  fun () ->
    (
# 352 "cparser.mly"
          ( getloc () )
# 878 "cparser.ml"
     : (Cast.location))

let _menhir_action_045 =
  fun () ->
    (
# 86 "cparser.mly"
                ( getloc () )
# 886 "cparser.ml"
     : (Cast.location))

let _menhir_action_046 =
  fun _1 ->
    (
# 235 "cparser.mly"
                                  ( _1 )
# 894 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_047 =
  fun _1 _3 _5 ->
    (
# 374 "cparser.mly"
    (
	    let loc = sup_locator _1 (fst _5) in
	    loc, CWHILE (_3, _5)
	   )
# 905 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_048 =
  fun _1 _3 _4 _6 ->
    (
# 380 "cparser.mly"
 ( 
          let loc = sup_locator _1 (fst _6) in
	  loc, CBLOCK ([], [(loc_of_expr _3, CEXPR _3);
			    loc, CWHILE (_4, _6)])
	)
# 917 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_049 =
  fun _1 _3 _4 _5 _7 ->
    (
# 387 "cparser.mly"
 ( 
          let loc = sup_locator _1 (fst _7) in
	  loc, CBLOCK ([], [(loc_of_expr _3, CEXPR _3);
			    loc, CWHILE (_4,
					 (sup_locator (loc_of_expr _5) (loc_of_expr _7),
					  CBLOCK ([], [_7; (loc_of_expr _5,
							    CEXPR _5)])))])
	)
# 932 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_050 =
  fun _1 ->
    (
# 401 "cparser.mly"
            ( _1, CRETURN None )
# 940 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_051 =
  fun _1 _2 ->
    (
# 403 "cparser.mly"
            ( sup_locator _1 (loc_of_expr _2), CRETURN (Some _2) )
# 948 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_052 =
  fun _1 ->
    (
# 239 "cparser.mly"
                                  ( _1 )
# 956 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_053 =
  fun _1 _3 ->
    (
# 241 "cparser.mly"
 ( let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF (_1, _3, (loc, CST 0))
	)
# 966 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_054 =
  fun _1 ->
    (
# 247 "cparser.mly"
                                 ( _1 )
# 974 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_055 =
  fun _1 _3 ->
    (
# 249 "cparser.mly"
 ( let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF (_1, (loc, CST 1), _3)
	)
# 984 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_056 =
  fun _1 ->
    (
# 159 "cparser.mly"
                          ( _1 )
# 992 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_057 =
  fun _1 _3 ->
    (
# 161 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_MUL, _1, _3)
	)
# 1002 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_058 =
  fun _1 _3 ->
    (
# 165 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_DIV, _1, _3)
	)
# 1012 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_059 =
  fun _1 _3 ->
    (
# 169 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_MOD, _1, _3)
	)
# 1022 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_060 =
  fun _1 ->
    (
# 313 "cparser.mly"
                        ( _1 )
# 1030 "cparser.ml"
     : (Cast.location))

let _menhir_action_061 =
  fun () ->
    (
# 74 "cparser.mly"
                              ( getloc () )
# 1038 "cparser.ml"
     : (Cast.location))

let _menhir_action_062 =
  fun _1 _2 ->
    (
# 421 "cparser.mly"
          ( let _, vname = _2 in CDECL(getloc(), vname, _1) )
# 1046 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_063 =
  fun () ->
    (
# 436 "cparser.mly"
                                  ( [] )
# 1054 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_064 =
  fun _2 ->
    (
# 437 "cparser.mly"
                                                      ( _2 )
# 1062 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_065 =
  fun _1 ->
    (
# 426 "cparser.mly"
          ( [_1] )
# 1070 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_066 =
  fun _1 _3 ->
    (
# 428 "cparser.mly"
          ( _3 :: _1 )
# 1078 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_067 =
  fun _1 ->
    (
# 432 "cparser.mly"
                         ( List.rev _1)
# 1086 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_068 =
  fun _1 ->
    (
# 90 "cparser.mly"
                             ( _1 )
# 1094 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_069 =
  fun _1 _3 ->
    (
# 92 "cparser.mly"
 ( let loc, var = _1 in
	  let loc1 = sup_locator loc _3 in
	    loc1, CALL (var, [])
	)
# 1105 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_070 =
  fun _1 _3 _4 ->
    (
# 97 "cparser.mly"
 ( let loc, var = _1 in
	  let loc1 = sup_locator loc _4 in
	    loc1, CALL (var, List.rev _3)
	)
# 1116 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_071 =
  fun _1 _2 ->
    (
# 102 "cparser.mly"
 ( sup_locator (loc_of_expr _1) _2, OP1 (M_POST_INC, _1) )
# 1124 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_072 =
  fun _1 _2 ->
    (
# 104 "cparser.mly"
 ( sup_locator (loc_of_expr _1) _2, OP1 (M_POST_DEC, _1) )
# 1132 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_073 =
  fun _1 ->
    (
# 64 "cparser.mly"
                     ( let loc, var = _1 in loc, VAR var )
# 1140 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_074 =
  fun _1 ->
    (
# 65 "cparser.mly"
                   ( let loc, cst = _1 in loc, CST cst )
# 1148 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_075 =
  fun _1 ->
    (
# 66 "cparser.mly"
                         ( let loc, s = _1 in loc, STRING s )
# 1156 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_076 =
  fun () ->
    (
# 67 "cparser.mly"
               ( getloc (), NULLPTR )
# 1164 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_077 =
  fun _2 ->
    (
# 68 "cparser.mly"
                                                    ( _2 )
# 1172 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_078 =
  fun _1 ->
    (
# 192 "cparser.mly"
                           ( _1 )
# 1180 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_079 =
  fun _1 _3 ->
    (
# 194 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LT, _1, _3)
	)
# 1190 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_080 =
  fun _1 _3 ->
    (
# 198 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LT, _3, _1)
	)
# 1200 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_081 =
  fun _1 _3 ->
    (
# 202 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LE, _1, _3)
	)
# 1210 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_082 =
  fun _1 _3 ->
    (
# 206 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LE, _3, _1)
	)
# 1220 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_083 =
  fun () ->
    (
# 397 "cparser.mly"
                ( getloc () )
# 1228 "cparser.ml"
     : (Cast.location))

let _menhir_action_084 =
  fun _1 _3 _5 ->
    (
# 356 "cparser.mly"
 ( 
          sup_locator _1 (fst _5), CIF (_3, _5,
					(getloc (), CBLOCK ([], [])))
	)
# 1239 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_085 =
  fun _1 _3 _5 _7 ->
    (
# 361 "cparser.mly"
 ( 
          sup_locator _1 (fst _7), CIF (_3, _5, _7)
	)
# 1249 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_086 =
  fun _1 _3 _5 _7 ->
    (
# 365 "cparser.mly"
 ( 
          sup_locator _1 (fst _7), CIF (_3, _5, _7)
	)
# 1259 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_087 =
  fun () ->
    (
# 350 "cparser.mly"
                    ( getloc () )
# 1267 "cparser.ml"
     : (Cast.location))

let _menhir_action_088 =
  fun _1 ->
    (
# 188 "cparser.mly"
                              ( _1 )
# 1275 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_089 =
  fun () ->
    (
# 149 "cparser.mly"
                        ( getloc (), STAR_CHR  )
# 1283 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_090 =
  fun _1 ->
    (
# 302 "cparser.mly"
            ( _1 )
# 1291 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_091 =
  fun _1 ->
    (
# 304 "cparser.mly"
            ( loc_of_expr _1, CEXPR _1 )
# 1299 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_092 =
  fun _1 ->
    (
# 306 "cparser.mly"
            ( _1 )
# 1307 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_093 =
  fun _1 ->
    (
# 308 "cparser.mly"
            ( _1 )
# 1315 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_094 =
  fun _1 ->
    (
# 310 "cparser.mly"
            ( _1 )
# 1323 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_095 =
  fun _1 ->
    (
# 338 "cparser.mly"
          ( [_1] )
# 1331 "cparser.ml"
     : (Cast.loc_code list))

let _menhir_action_096 =
  fun _1 _2 ->
    (
# 340 "cparser.mly"
          ( _2 :: _1 )
# 1339 "cparser.ml"
     : (Cast.loc_code list))

let _menhir_action_097 =
  fun _1 ->
    (
# 78 "cparser.mly"
                         ( getloc (), _1 )
# 1347 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_098 =
  fun _1 _2 ->
    (
# 80 "cparser.mly"
            ( 
              let l, s = _2 in
              let s2 = _1 in
              (getloc (), s2^s)
            )
# 1359 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_099 =
  fun () ->
    (
# 146 "cparser.mly"
                        ( getloc (), SUB_CHR   )
# 1367 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_100 =
  fun () ->
    (
# 148 "cparser.mly"
                        ( getloc (), TILDE_CHR )
# 1375 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_101 =
  fun () ->
    (
# 297 "cparser.mly"
                  ( TINT )
# 1383 "cparser.ml"
     : (Cast.ctyp))

let _menhir_action_102 =
  fun _1 ->
    (
# 298 "cparser.mly"
                           ( TPTR(_1) )
# 1391 "cparser.ml"
     : (Cast.ctyp))

let _menhir_action_103 =
  fun _1 ->
    (
# 116 "cparser.mly"
                             ( _1 )
# 1399 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_104 =
  fun _1 _2 ->
    (
# 118 "cparser.mly"
 ( sup_locator _1 (loc_of_expr _2), OP1 (M_PRE_INC, _2) )
# 1407 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_105 =
  fun _1 _2 ->
    (
# 120 "cparser.mly"
 ( sup_locator _1 (loc_of_expr _2), OP1 (M_PRE_DEC, _2) )
# 1415 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_106 =
  fun _1 _2 ->
    (
# 122 "cparser.mly"
 ( 
          let loc, c = _1 in
          let loc' = sup_locator loc (loc_of_expr _2) in
	  match c with
	      ADD_CHR -> _2
	    | SUB_CHR -> loc', OP1 (M_MINUS, _2)
	    | BANG_CHR -> loc', EIF (_2, (loc', CST 0), (loc', CST 1))
            | TILDE_CHR -> loc', OP1 (M_NOT, _2)
            | STAR_CHR -> loc', OP1 (M_DEREF, _2)
            | AMP_CHR -> loc', OP1 (M_ADDR, _2)
	    | _ -> raise (Parse_error (loc, "unknown unary operator"))
	)
# 1434 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_107 =
  fun _1 ->
    (
# 137 "cparser.mly"
                    ( _1 )
# 1442 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_108 =
  fun _1 ->
    (
# 138 "cparser.mly"
                    ( _1 )
# 1450 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_109 =
  fun _1 ->
    (
# 139 "cparser.mly"
                    ( _1 )
# 1458 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_110 =
  fun _1 ->
    (
# 140 "cparser.mly"
                    ( _1 )
# 1466 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_111 =
  fun _1 ->
    (
# 141 "cparser.mly"
                    ( _1 )
# 1474 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_112 =
  fun _1 ->
    (
# 142 "cparser.mly"
                    ( _1 )
# 1482 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_113 =
  fun () ->
    (
# 370 "cparser.mly"
                ( getloc () )
# 1490 "cparser.ml"
     : (Cast.location))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD_CHR ->
        "ADD_CHR"
    | AMP_CHR ->
        "AMP_CHR"
    | AND_OP ->
        "AND_OP"
    | BANG_CHR ->
        "BANG_CHR"
    | CLOSE_ANGLE_CHR ->
        "CLOSE_ANGLE_CHR"
    | CLOSE_BRACE_CHR ->
        "CLOSE_BRACE_CHR"
    | CLOSE_PAREN_CHR ->
        "CLOSE_PAREN_CHR"
    | COLON_CHR ->
        "COLON_CHR"
    | COMMA_CHR ->
        "COMMA_CHR"
    | CONSTANT _ ->
        "CONSTANT"
    | DEC_OP ->
        "DEC_OP"
    | DIV_CHR ->
        "DIV_CHR"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ_CHR ->
        "EQ_CHR"
    | EQ_OP ->
        "EQ_OP"
    | FOR ->
        "FOR"
    | GE_OP ->
        "GE_OP"
    | IDENTIFIER _ ->
        "IDENTIFIER"
    | IF ->
        "IF"
    | INC_OP ->
        "INC_OP"
    | INTEGER ->
        "INTEGER"
    | LE_OP ->
        "LE_OP"
    | MOD_CHR ->
        "MOD_CHR"
    | NE_OP ->
        "NE_OP"
    | NULL ->
        "NULL"
    | OPEN_ANGLE_CHR ->
        "OPEN_ANGLE_CHR"
    | OPEN_BRACE_CHR ->
        "OPEN_BRACE_CHR"
    | OPEN_PAREN_CHR ->
        "OPEN_PAREN_CHR"
    | OR_OP ->
        "OR_OP"
    | QUES_CHR ->
        "QUES_CHR"
    | RETURN ->
        "RETURN"
    | SEMI_CHR ->
        "SEMI_CHR"
    | STAR_CHR ->
        "STAR_CHR"
    | STRING_LITERAL _ ->
        "STRING_LITERAL"
    | SUB_CHR ->
        "SUB_CHR"
    | TILDE_CHR ->
        "TILDE_CHR"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_172 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      MenhirBox_file _v
  
  let rec _menhir_goto_file : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState173 ->
          _menhir_run_174 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_172 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_174 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_external_declaration -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      let MenhirCell1_external_declaration (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_039 _1 _2 in
      _menhir_goto_file _menhir_stack _v _menhir_s
  
  let _menhir_run_002 : type  ttv_stack. ttv_stack -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_s ->
      let _v = _menhir_action_038 () in
      _menhir_goto_file _menhir_stack _v _menhir_s
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_101 () in
      _menhir_goto_type_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_specifier : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState015 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_158 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENTIFIER _v_0 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState158
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _v = _menhir_action_102 _1 in
      _menhir_goto_type_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_043 _1 in
      _menhir_goto_identifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_identifier : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState003 ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_054 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState055 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CLOSE_PAREN_CHR ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DEC_OP | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | INC_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_073 _1 in
          _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_100 () in
      let _1 = _v in
      let _v = _menhir_action_110 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unary_operator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_unary_operator (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState042
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState042
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState042
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_099 () in
      let _1 = _v in
      let _v = _menhir_action_108 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING_LITERAL _v_0 ->
          let _menhir_stack = MenhirCell1_STRING_LITERAL (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState029
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DEC_OP | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | INC_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_097 _1 in
          _menhir_goto_string_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_string_literal : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState165 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_046 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_primary_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_postfix_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | INC_OP ->
          let _menhir_stack = MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_103 _1 in
          _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_045 () in
      _menhir_goto_inc_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_inc_op : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_052 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_inc_op (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState052
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState052
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState052
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_089 () in
      let _1 = _v in
      let _v = _menhir_action_111 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState034 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_076 () in
      _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_023 () in
      _menhir_goto_dec_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dec_op : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState025 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_065 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_dec_op (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState065
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState065
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState065
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_022 _1 in
      let _1 = _v in
      let _v = _menhir_action_074 _1 in
      _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_011 () in
      let _1 = _v in
      let _v = _menhir_action_109 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_040 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_005 () in
      let _1 = _v in
      let _v = _menhir_action_112 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_001 () in
      let _1 = _v in
      let _v = _menhir_action_107 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_postfix_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_072 _1 _2 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_postfix_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_071 _1 _2 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unary_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState065 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState165 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState108 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_066 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_dec_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_dec_op (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_105 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_057 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EQ_CHR ->
          let _menhir_stack = MenhirCell1_unary_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState058 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DIV_CHR | EQ_OP | GE_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_012 _1 in
          _menhir_goto_cast_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_cast_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState042 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState072 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState064 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_117 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_operator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_operator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_106 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_multiplicative_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState080 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_081 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_003 _1 _3 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState064 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState072 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState074 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_additive_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState078 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState080 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_088 _1 in
          _menhir_goto_shift_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_shift_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_087 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_080 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_relational_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_098 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_029 _1 _3 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_061 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState061 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState082 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState084 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_equality_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NE_OP ->
          let _menhir_stack = MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState095 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EQ_OP ->
          let _menhir_stack = MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState097 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_006 _1 in
          let _1 = _v in
          let _v = _menhir_action_031 _1 in
          let _1 = _v in
          let _v = _menhir_action_046 _1 in
          _menhir_goto_inclusive_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_inclusive_or_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_100 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_goto_logical_and_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_logical_and_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState108 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_109 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND_OP ->
          let _menhir_stack = MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_055 _1 _3 in
          _menhir_goto_logical_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_091 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState091 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_logical_or_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | QUES_CHR ->
          let _menhir_stack = MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState089 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | OR_OP ->
          let _menhir_stack = MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState108 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_020 _1 in
          _menhir_goto_conditional_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_conditional_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState105 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_106 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COLON_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_021 _1 _3 _5 in
      _menhir_goto_conditional_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_103 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_goto_assignment_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_assignment_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState114 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState055 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_115 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_argument_expression_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_008 _1 _3 in
      _menhir_goto_argument_expression_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_argument_expression_list : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_argument_expression_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_COMMA_CHR (_menhir_stack, MenhirState113) in
          let _menhir_s = MenhirState114 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_015 () in
      _menhir_goto_close_paren _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_close_paren : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState144 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState055 ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_152 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState152
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | RETURN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | IF ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState152
      | FOR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState152
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_113 () in
      let _menhir_stack = MenhirCell1_whilekw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_s = MenhirState124 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_087 () in
      let _1 = _v in
      let _v = _menhir_action_034 _1 in
      _menhir_goto_expression_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_148 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState159 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState152 ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState146 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState139 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState126 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_163 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_095 _1 in
      _menhir_goto_statement_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_statement_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_165 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState165
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | RETURN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | IF ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState165
      | FOR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState165
      | CLOSE_BRACE_CHR ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState165
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_083 () in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | STRING_LITERAL _v_0 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState130
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_050 _1 in
          _menhir_goto_jump_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | NULL ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | INC_OP ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | IDENTIFIER _v_1 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState130
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | CONSTANT _v_2 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState130
      | BANG_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | AMP_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | _ ->
          _eRR ()
  
  and _menhir_goto_jump_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_094 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_023 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_061 () in
      let _1 = _v in
      let _v = _menhir_action_060 _1 in
      let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState025
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | RETURN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | IF ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState025
      | FOR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState025
      | CLOSE_BRACE_CHR ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | _ ->
          _eRR ()
  
  and _menhir_run_120 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_044 () in
      let _menhir_stack = MenhirCell1_ifkw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_s = MenhirState137 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_040 () in
      let _menhir_stack = MenhirCell1_forkw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_s = MenhirState142 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI_CHR ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_122 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_014 () in
      let _1 = _v in
      let _v = _menhir_action_013 _1 in
      _menhir_goto_close_block _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_close_block : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState025 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState165 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState159 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_170 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_016 _1 _2 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_compound_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState022 ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState155 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState139 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_171 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_function_declarator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_function_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_042 _1 _2 in
      let _1 = _v in
      let _v = _menhir_action_036 _1 in
      _menhir_goto_external_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_external_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_external_declaration (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | EOF ->
          _menhir_run_002 _menhir_stack MenhirState173
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_compound_statement (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_086 _1 _3 _5 _7 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_selection_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState155 ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_156 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_compound_statement (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_085 _1 _3 _5 _7 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_129 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_092 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_154 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_compound_statement (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState155 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | OPEN_BRACE_CHR ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR | AMP_CHR | BANG_CHR | CLOSE_BRACE_CHR | CONSTANT _ | DEC_OP | FOR | IDENTIFIER _ | IF | INC_OP | NULL | OPEN_BRACE_CHR | OPEN_PAREN_CHR | RETURN | SEMI_CHR | STAR_CHR | STRING_LITERAL _ | SUB_CHR | TILDE_CHR | WHILE ->
          let _1 = _v in
          let _v = _menhir_action_090 _1 in
          _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_168 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_declaration_list (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_018 _1 _2 _3 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_166 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_statement_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_declaration_list (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_019 _1 _2 _3 _4 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_162 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_statement_list (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_017 _1 _2 _3 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_159 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState159
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | RETURN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | IF ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState159
      | FOR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState159
      | CLOSE_BRACE_CHR ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | _ ->
          _eRR ()
  
  and _menhir_run_160 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_statement_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_096 _1 _2 in
      _menhir_goto_statement_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_153 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_forkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_048 _1 _3 _4 _6 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_iteration_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_093 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_147 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_forkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_049 _1 _3 _4 _5 _7 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_140 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_084 _1 _3 _5 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_127 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_whilekw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_047 _1 _3 _5 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_144 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression_statement (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState144
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState144
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState144
      | CLOSE_PAREN_CHR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression_statement (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState143
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState143
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState143
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | _ ->
          _eRR ()
  
  and _menhir_run_146 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState146
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | RETURN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | IF ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState146
      | FOR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState146
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState126
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | RETURN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | IF ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState126
      | FOR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState126
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState126
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_argument_expression_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_identifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_070 _1 _3 _4 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_111 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_identifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_069 _1 _3 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_112 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_007 _1 in
      _menhir_goto_argument_expression_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_110 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_010 _1 _3 in
      _menhir_goto_assignment_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_107 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState126 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_149 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_035 _1 in
          _menhir_goto_expression_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression as 'stack) -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COMMA_CHR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState102 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_145 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | CLOSE_PAREN_CHR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | _ ->
          _eRR ()
  
  and _menhir_run_138 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
      | CLOSE_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, MenhirState138) in
          let _menhir_s = MenhirState139 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI_CHR ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_BRACE_CHR ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_132 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_return as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_return (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_051 _1 _2 in
          _menhir_goto_jump_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState132
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState125
      | CLOSE_PAREN_CHR ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState125
      | _ ->
          _eRR ()
  
  and _menhir_run_118 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | CLOSE_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_077 _2 in
          _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | COLON_CHR ->
          let _menhir_stack = MenhirCell1_COLON_CHR (_menhir_stack, MenhirState101) in
          let _menhir_s = MenhirState105 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_104 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_033 _1 _3 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_090 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND_OP ->
          let _menhir_stack = MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_054 _1 in
          _menhir_goto_logical_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_092 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_053 _1 _3 in
      _menhir_goto_logical_and_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_096 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_030 _1 _3 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_060 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_028 _1 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_082 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_083 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_081 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_062 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_079 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_059 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_078 _1 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_079 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_004 _1 _3 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_002 _1 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_058 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_059 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_071 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_057 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_053 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_inc_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_inc_op (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_104 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_043 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_goto_cast_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_030 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_STRING_LITERAL -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_STRING_LITERAL (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_098 _1 _2 in
      _menhir_goto_string_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_010 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_027 _1 in
      _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_declarator : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState158 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState003 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState009 ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_019 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_024 _1 _2 in
          _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_175 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_037 _1 in
      _menhir_goto_external_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_169 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_025 _1 in
      _menhir_goto_declaration_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_declaration_list : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | TILDE_CHR ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | SUB_CHR ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | STRING_LITERAL _v_0 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState164
      | STAR_CHR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | SEMI_CHR ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | RETURN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | NULL ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | INC_OP ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | IF ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | IDENTIFIER _v_1 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState164
      | FOR ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | DEC_OP ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | CONSTANT _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState164
      | CLOSE_BRACE_CHR ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | BANG_CHR ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | AMP_CHR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | ADD_CHR ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | _ ->
          _eRR ()
  
  and _menhir_run_167 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_026 _1 _2 in
      _menhir_goto_declaration_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_011 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_062 _1 _2 in
      _menhir_goto_parameter_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameter_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState007 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState015 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_017 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_065 _1 in
      _menhir_goto_parameter_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameter_list : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState015 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INTEGER ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR ->
          let _1 = _v in
          let _v = _menhir_action_067 _1 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_064 _2 in
          _menhir_goto_parameter_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_parameter_declarator : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_identifier (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_041 _1 _2 _3 in
      let _menhir_stack = MenhirCell1_function_declarator (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_BRACE_CHR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_parameter_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_066 _1 _3 in
      _menhir_goto_parameter_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_006 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INTEGER ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState007
          | CLOSE_PAREN_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_063 () in
              _menhir_goto_parameter_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_027 _1 in
          _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_009 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENTIFIER _v_0 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState009
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENTIFIER _v_0 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState003
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState000 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EOF ->
          _menhir_run_002 _menhir_stack _menhir_s
      | _ ->
          _eRR ()
  
end

let file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_file v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 453 "cparser.mly"
  

# 5098 "cparser.ml"
