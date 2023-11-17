

(* Programme principal *)

open Format
open Lexing
open Clexer
open Cparser
open Usage
open Ctyping

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Cparser.file Clexer.ctoken lb in
    close_in c;

    if Usage.debug then begin
      let ast_dot_file = open_out (Filename.chop_suffix file ".c" ^ "_ast.dot") in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_ast f (not !no_pretty));
      close_out ast_dot_file;
      (* eprintf "DEBUG\n@."; *)
    end;

    if !Usage.parse_only then exit 0;


    Ctyping.check_file f;

    let code = Ccompile.check_file f in
    let oc = open_out (Filename.chop_suffix file ".c" ^ ".s") in
    Printf.fprintf oc "%s\n" code;
    close_out oc;



  with
    | Clexer.Lexing_error s ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "Lexical error: %s\n@." s;
      exit 1
    | Cparser.Error ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "Syntax error\n@.";
      exit 1
    | Ctyping.Error (l, msg) ->
      report_loc l;
      eprintf "Typing error: %s\n@." msg;
      exit 1
    | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2

