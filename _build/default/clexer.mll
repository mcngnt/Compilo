{

(*
 *	Copyright (C) 2023 by Laboratoire Méthodes Formelles (LMF),
 *	UMR 9021 Université Paris-Saclay, CNRS et ENS Paris-Saclay.
 *	Modified by Mihaela Sighireanu.
 *
 *	Copyright (C) 2005, 2006 by Laboratoire Spécification et Vérification (LSV),
 *	UMR 8643 CNRS & ENS Cachan.
 *	Written by Jean Goubault-Larrecq.  Derived from the csur project.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. Neither the author nor its employer is responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 *	4. This software is restricted to non-commercial use only.  Commercial
 *		use is subject to a specific license, obtainable from LMF.
 * 
*)

(* Analyse lexicale d'un sous-ensemble (tres) reduit de C.
 *)

open Lexing
open Cast
open Cparser

exception Lexing_error of string

let string_buf = Buffer.create 256

let string_iter f s = (* = String.iter; pas present en OCaml 2.04. *)
	let n = String.length s
	in for i=0 to n-1 do f (s.[i]) done

let count yytext =
	(oldcline := !cline; oldccol := !ccol;
	string_iter (fun c -> match c with
			'\n' -> (cline := !cline+1; ccol := 0)
                      (* | '\t' -> (ccol := !ccol + 8 - (!ccol mod 8)) *)
                      | _ -> ccol := !ccol+1) yytext)

let parse_hex yytext tend =
	let n = ref 0
	in let len = String.length yytext-tend
	in ((for i=2 to len-1 do
	     let c = yytext.[i] in
	     match c with
	         '0'..'9' -> n := 16 * !n + (int_of_char c - int_of_char '0')
               | 'a'..'f' -> n := 16 * !n + (int_of_char c + 10 - int_of_char 'a')
               | 'A'..'F' -> n := 16 * !n + (int_of_char c + 10 - int_of_char 'A')
	       | _ -> raise (Lexing_error ("invalid hexadecimal number " ^ yytext))
	     done);
	    !n)

let parse_oct yytext start tend =
	let n = ref 0
	in let len = String.length yytext-tend
	in ((for i=start to len-1 do
	     let c = yytext.[i] in
	     match c with
	         '0'..'7' -> n := 8 * !n + (int_of_char c - int_of_char '0')
	       | _ -> raise (Lexing_error ("invalid octal number " ^ yytext))
	     done);
	    !n)

let parse_dec yytext tend =
	let n = ref 0
	in let len = String.length yytext-tend
	in ((for i=0 to len-1 do
	     let c = yytext.[i] in
	     match c with
	         '0'..'9' -> n := 10 * !n + (int_of_char c - int_of_char '0')
	       | _ -> raise (Lexing_error ("invalid number " ^ yytext))
	    done);
	    !n)

}

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let hex    = ['a'-'f' 'A'-'F' '0'-'9']
let expo   = ['E' 'e'] ['+' '-']? digit+
let fs     = ['f' 'F' 'l' 'L']
let is     = ['u' 'U' 'l' 'L']*

rule ctoken = parse
    "/*" { count (Lexing.lexeme lexbuf); comment lexbuf; ctoken lexbuf }
  | "//" [^ '\n']* '\n' { count (Lexing.lexeme lexbuf); ctoken lexbuf }
  | "else" { count (Lexing.lexeme lexbuf); ELSE }
  | "for" { count (Lexing.lexeme lexbuf); FOR }
  | "if" { count (Lexing.lexeme lexbuf); IF }
  | "int" { count (Lexing.lexeme lexbuf); INTEGER }
  | "return" { count (Lexing.lexeme lexbuf); RETURN }
  | "while" { count (Lexing.lexeme lexbuf); WHILE }
  | "NULL" {count (Lexing.lexeme lexbuf); NULL }
  | letter (letter | digit)* { count (Lexing.lexeme lexbuf);
		let yytext = Lexing.lexeme lexbuf in
		    IDENTIFIER yytext
                    } 
  | '0' ['x' 'X'] hex+ { count (Lexing.lexeme lexbuf);
			CONSTANT (parse_hex (Lexing.lexeme lexbuf) 0) }

  | '0' ['0'-'7']+ { count (Lexing.lexeme lexbuf);
			CONSTANT (parse_oct (Lexing.lexeme lexbuf) 1 0) }

  | digit+ { count (Lexing.lexeme lexbuf);
			CONSTANT (parse_dec (Lexing.lexeme lexbuf) 0) }

  | '\'' [^ '\'' '\\'] '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT (int_of_char (Lexing.lexeme_char lexbuf 1)) }
  | '\'' '\\' ['0'-'7'] ['0'-'7']? ['0'-'7']? '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT (parse_oct (Lexing.lexeme lexbuf) 2 1) }
  | '\'' '\\' 'a' '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT 7 (* bell, ^G *) }
  | '\'' '\\' 'b' '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT (int_of_char '\b') }
  | '\'' '\\' 'f' '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT 12 (* form feed, ^L *) }
  | '\'' '\\' 'n' '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT (int_of_char '\n') }
  | '\'' '\\' 'r' '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT (int_of_char '\r') }
  | '\'' '\\' 't' '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT (int_of_char '\t')
				 (* bell, ^G *) }
  | '\'' '\\' 'v' '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT 11 (* vertical tab, ^K *) }
  | '\'' '\\' _ '\'' { count (Lexing.lexeme lexbuf);
			CONSTANT (int_of_char (Lexing.lexeme_char lexbuf 2)) }
  | "\"" 
      { 
        count (Lexing.lexeme lexbuf); Buffer.reset string_buf;
        string lexbuf;
        STRING_LITERAL (Buffer.contents string_buf)
      }
  | "++"  { count (Lexing.lexeme lexbuf); INC_OP }
  | "--"  { count (Lexing.lexeme lexbuf); DEC_OP }
  | "&&"  { count (Lexing.lexeme lexbuf); AND_OP }
  | "||"  { count (Lexing.lexeme lexbuf); OR_OP }
  | "<="  { count (Lexing.lexeme lexbuf); LE_OP }
  | ">="  { count (Lexing.lexeme lexbuf); GE_OP }
  | "=="  { count (Lexing.lexeme lexbuf); EQ_OP }
  | "!="  { count (Lexing.lexeme lexbuf); NE_OP }
  | ";"   { count (Lexing.lexeme lexbuf); SEMI_CHR }
  | ("{" | "<%") { count (Lexing.lexeme lexbuf); OPEN_BRACE_CHR }
  | ("}" | "%>") { count (Lexing.lexeme lexbuf); CLOSE_BRACE_CHR }
  | "," { count (Lexing.lexeme lexbuf); COMMA_CHR }
  | ":" { count (Lexing.lexeme lexbuf); COLON_CHR }
  | "=" { count (Lexing.lexeme lexbuf); EQ_CHR }
  | "(" { count (Lexing.lexeme lexbuf); OPEN_PAREN_CHR }
  | ")" { count (Lexing.lexeme lexbuf); CLOSE_PAREN_CHR }
  | "!" { count (Lexing.lexeme lexbuf); BANG_CHR }
  | "~" { count (Lexing.lexeme lexbuf); TILDE_CHR }
  | "+" { count (Lexing.lexeme lexbuf); ADD_CHR }
  | "-" { count (Lexing.lexeme lexbuf); SUB_CHR }
  | "*" { count (Lexing.lexeme lexbuf); STAR_CHR }
  | "&" { count (Lexing.lexeme lexbuf); AMP_CHR }
  | "/" { count (Lexing.lexeme lexbuf); DIV_CHR }
  | "%" { count (Lexing.lexeme lexbuf); MOD_CHR }
  | "<" { count (Lexing.lexeme lexbuf); OPEN_ANGLE_CHR }
  | ">" { count (Lexing.lexeme lexbuf); CLOSE_ANGLE_CHR }
  | "?" { count (Lexing.lexeme lexbuf); QUES_CHR }
  | '#' { count (Lexing.lexeme lexbuf); line lexbuf }
  | [' ' '\t' '\012' '\013' '\n' '\014']+ { count (Lexing.lexeme lexbuf); ctoken lexbuf }
  | _ { raise (Lexing_error ("bad character '" ^ (Lexing.lexeme lexbuf) ^ "'")) }
  | eof { EOF }
and comment = parse
    "*/" { count (Lexing.lexeme lexbuf) }
  | [^ '*']* { count (Lexing.lexeme lexbuf); comment lexbuf }
  | eof { raise (Lexing_error ("end of file reached inside comment")) }
and string = parse
    '"' { () }
  | '\n'+ { string lexbuf }
  | '\\' ['0'-'7'] ['0'-'7']? ['0'-'7']? { Buffer.add_char string_buf (Char.chr (parse_oct (Lexing.lexeme lexbuf) 1 0)); string lexbuf }
  | '\\' 'a' { Buffer.add_char string_buf '\007'; string lexbuf }
  | '\\' 'b' { Buffer.add_char string_buf '\b'; string lexbuf }
  | '\\' 'f' { Buffer.add_char string_buf '\014'; string lexbuf }
  | '\\' 'n' { Buffer.add_char string_buf '\n'; string lexbuf }
  | '\\' 'r' { Buffer.add_char string_buf '\r'; string lexbuf }
  | '\\' 't' { Buffer.add_char string_buf '\t'; string lexbuf }
  | '\\' 'v' { Buffer.add_char string_buf '\013'; string lexbuf }
  | '\\' _ { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 1); string lexbuf }
  | [^ '\\' '\n' '"']+ { Buffer.add_string string_buf (Lexing.lexeme lexbuf); string lexbuf }
  | _ { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0); string lexbuf }
  | eof { raise (Lexing_error ("end of file reached inside string literal")) }
and line = parse
    ['0'-'9']+ { cline := parse_dec (Lexing.lexeme lexbuf) 0 - 1; line2 lexbuf }
  | [' ' '\t']+ { count (Lexing.lexeme lexbuf); line lexbuf }
  | '\n' { count (Lexing.lexeme lexbuf); ctoken lexbuf }
  | "\"" { count (Lexing.lexeme lexbuf); Buffer.reset string_buf;
        string lexbuf;
	cfile := Buffer.contents string_buf;
	ctoken lexbuf
      }
  | eof { raise (Lexing_error ("end of file reached inside # directive")) }
and line2 = parse
    [' ' '\t']+ { count (Lexing.lexeme lexbuf); line2 lexbuf }
  | '\n' { count (Lexing.lexeme lexbuf); ctoken lexbuf }
  | "\"" { count (Lexing.lexeme lexbuf); Buffer.reset string_buf;
        string lexbuf;
	cfile := Buffer.contents string_buf;
	line3 lexbuf
      }
  | eof { raise (Lexing_error ("end of file reached inside # directive")) }
and line3 = parse
    '\n' { count (Lexing.lexeme lexbuf); ctoken lexbuf }
  |  _ { count (Lexing.lexeme lexbuf); line3 lexbuf }
  | eof  { raise (Lexing_error ("end of file reached inside # directive")) }
