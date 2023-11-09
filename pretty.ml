open Cast
open Printf
open Tast

let is_pretty = ref true

let fold_string f list = String.concat "" (List.map f list)
let fold_children f list parent_id = fold_string (fun item -> f item parent_id) list

let fold_hashtbl_children f hashtbl parent_id =
  fold_string (fun item -> f item parent_id) (Hashtbl.fold (fun k v acc -> (k, v) :: acc) hashtbl [])

let get_node_id, reset_node_id =
  let id = ref 0 in
  ( fun name -> ( incr id; sprintf "%s_%d" name !id ) ),
  fun () -> id := 0

let link_nodes id_from id_to = sprintf "%s -> %s\n" id_from id_to

let html_attribute balise attribute name =  sprintf "<%s %s>%s</%s>" balise attribute name balise
let html balise name = html_attribute balise "" name

let draw_node id =
  let draw_node_no_pretty id name fields children =
    if fields = [] then "" else sprintf "%s:%s\n" id (String.concat "" fields) in

  let draw_node_pretty id name fields children =
    let colspan = sprintf "colspan = '%d'" (max 1 (List.length children)) in
    let rowfield field = html "tr" (html_attribute "td" colspan field) in
    let rowchild child = html_attribute "td" (sprintf "port='%s'" child) child in
    let table = html_attribute "table" "border='0' cellborder='1' cellspacing='0' cellpadding='4'" (
        html "tr" (html_attribute "td" colspan (html "b" name)) ^
        ( if fields = [] then "" else fold_string rowfield fields ) ^
        ( if children = [] then "" else html "tr" (fold_string rowchild children) )
      )
    in sprintf "%s [label=<%s>]\n" id table in

  if !is_pretty then draw_node_pretty id else draw_node_no_pretty id

let create_node parent_id name fields children =
  let node_id = get_node_id name in
  let node_children = List.map (fun (child, _) -> child) children
  in draw_node node_id name fields node_children ^
     link_nodes parent_id node_id ^
     fold_string (fun (child, f) -> f (sprintf "%s:%s" node_id child)) children

let draw_root_ast f =
  reset_node_id ();
  let ast = draw_node "root" "*" [] [] ^ f "root" in

  if !is_pretty then
    "digraph ast {\n" ^
    "node [shape=plaintext];\n" ^
    ast ^ "}"
  else
    ast


let binop = function
  | S_ADD -> "+"
  | S_SUB -> "-"
  | S_MUL -> "*"
  | S_DIV -> "/"
  | S_MOD -> "%"

let cmpop = function
  | C_LT -> "&lt;"
  | C_LE -> "&le;"
  | C_EQ -> "=="

let unop = function
  | M_MINUS -> "-"
  | M_NOT -> "!"
  | M_POST_INC -> "++"
  | M_POST_DEC -> "--"
  | M_PRE_INC -> "++"
  | M_PRE_DEC -> "--"
  | M_DEREF -> "*"
  | M_ADDR -> "&"

let rec get_ast_typ ptyp parent_id =
  let create_node = create_node parent_id in
  match ptyp with
  | TINT -> create_node "Tint" [] []
  | TPTR ptyp -> create_node "Tptr" [] [ ("ptyp", get_ast_typ ptyp) ]

let rec get_ast_expr lexpr parent_id =
  let create_node = create_node parent_id in
  let _, expr = lexpr in
  match expr with
  | VAR s -> create_node "VAR" [ sprintf "name = \"%s\"" s ] []
  | CST i -> create_node "CST" [ sprintf "int = \"%d\"" i ] []
  | STRING s -> create_node "STRING" [ sprintf "string = \"%s\"" s ] []
  | NULLPTR -> create_node "NULL" [] []
 
  | SET_VAR (s, lexpr) -> create_node "SET_VAR" [ sprintf "vg = \"%s\"" s ] [
      ("pexpr_left", get_ast_expr lexpr);
    ]

  | SET_VAL (s, lexpr) -> create_node "SET_VAL" [ sprintf "vg = *\"%s\"" s ] [
      ("pexpr_left", get_ast_expr lexpr);
    ]
      
  | CALL (s, pexprs) -> create_node "CALL" [ sprintf "fname = \"%s\"" s ] [
      ("pexpr_list", fold_children get_ast_expr pexprs)
    ]

  | OP1 (op, pexpr) -> create_node "OP1" [ "unop = \"" ^ unop op ^ "\"" ] [
      ("pexpr", get_ast_expr pexpr);
    ]
      
  | OP2 (op, pexpr_left, pexpr_right) ->
    create_node "OP2" [ "binop = \"" ^ binop op ^ "\"" ] [
      ("pexpr_left", get_ast_expr pexpr_left);
      ("pexpr_right", get_ast_expr pexpr_right)
    ]

  | CMP (op, pexpr_left, pexpr_right) ->
    create_node "CMP" [ "cmpop = \"" ^ cmpop op ^ "\"" ] [
      ("pexpr_left", get_ast_expr pexpr_left);
      ("pexpr_right", get_ast_expr pexpr_right)
    ]
 
  | EIF (cond, if_pexpr, else_pexpr) -> create_node "EIF" [] [
      ("pexpr_cond", get_ast_expr cond);
      ("pexpr_if", get_ast_expr if_pexpr);
      ("pexpr_else", get_ast_expr else_pexpr)
    ]

  | ESEQ pexprs -> create_node "ESEQ" [] [
      ("pexpr_list", fold_children get_ast_expr pexprs)
    ]

let rec get_ast_code lcode parent_id =
  let create_node = create_node parent_id in
  let _, blk = lcode in
  match blk with
  | CBLOCK (decl, stmts) ->
    create_node "CBLOCK" [] [
      ("var_decl_list", fold_children get_ast_pdecl decl);
      ("loc_code_list", fold_children get_ast_code stmts)
    ]
  | CEXPR pexpr ->
    create_node "CEXPR" [] [
      ("pexpr", get_ast_expr pexpr)
    ]
  | CIF (cond, if_lcode, else_lcode) ->
    create_node "CIF" [] [
      ("loc_expr_cond", get_ast_expr cond);
      ("loc_code_if", get_ast_code if_lcode);
      ("loc_code_else", get_ast_code else_lcode)
    ]
  | CWHILE (cond, while_block) ->
    create_node "CWHILE" [] [
      ("loc_expr_cond", get_ast_expr cond);
      ("loc_code_while", get_ast_code while_block)
    ]
  | CRETURN (None) ->
    create_node "CRETURN" [] []
  | CRETURN (Some(lexpr)) ->
    create_node "CRETURN" [] [
      ("loc_expr", get_ast_expr lexpr);
    ]

and get_ast_ptyp ptyp parent_id =
  let create_node = create_node parent_id in
  match ptyp with
  | TINT -> create_node "TINT" [] []
  | TPTR (typ) -> 
    create_node "TPTR" [] [
      ("ptyp", get_ast_ptyp typ)
    ]
and get_ast_pdecl pdecl parent_id =
  let create_node = create_node parent_id in
  match pdecl with
  | CFUN (_, f_name, f_params, f_rtyp, f_code) ->
    create_node "CFUN" [ "f_name = \"" ^ f_name ^ "\"" ] [
      ("f_params", fold_children get_ast_pdecl f_params);
      ("f_rtyp", get_ast_ptyp f_rtyp);
      ("f_code", get_ast_code f_code)
    ]

  | CDECL (_, v_name, v_typ) ->
    create_node "CDECL" [ "v_name = \"" ^ v_name ^ "\"" ] [
      ("v_typ", get_ast_ptyp v_typ);
    ]


let get_dot_ast pdecls flag_is_pretty =
  is_pretty := flag_is_pretty;
  draw_root_ast (fold_children get_ast_pdecl pdecls)

let get_dot_tast pdecls flag_is_pretty =
  is_pretty := flag_is_pretty;
  draw_root_ast (fold_children get_ast_pdecl pdecls)