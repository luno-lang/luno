(**
  Lexer for Tsuki
 *)

{
open Batteries
open Lexing
open Parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let ident = (alpha) (alpha|digit|'_')*

let whitespace = [' ' '\t']+
let eol = ['\r' '\n'] | '\r' '\n'

rule lex_token = parse
  | "+"  {PLUS}
  | "-"  {MINUS}
  | "*"  {STAR}
  | "/"  {SLASH}
  | "("  {LPAREN}
  | ")"  {RPAREN}
  | "["  {LBRACK}
  | "]"  {RBRACK}
  | ","  {COMMA}
  | ":"  {COLON}
  | "="  {ASSIGN}
  | "==" {EQ}
  | "~=" {NOT_EQ}
  | "<"  {LT}
  | ">"  {GT}
  | "<=" {LT_EQ}
  | ">=" {GT_EQ}
  | '"'  {lex_string (Buffer.create 16) lexbuf}
  | "#" {lex_comment lexbuf}
  
  | "fn" {FN}
  | "if" {IF}
  | "else" {ELSE}
  | "for" {FOR}
  | "of" {OF}
  | "while" {WHILE}
  | "then" {THEN}
  | "var" {VAR}
  | "end" {END}
  | int as i {INT (int_of_string i)}
  | ident as s {IDENT s}
  | whitespace {lex_token lexbuf}
  | eol {new_line lexbuf; lex_token lexbuf}
  | eof {EOF}
  | _ { raise (SyntaxError ("lexer: illegal character " ^ Lexing.lexeme lexbuf)) }

and lex_string buf = parse
  | '"' {STRING (Buffer.contents buf)}
  | '\\' '\n' {Buffer.add_char buf '\n'; lex_string buf lexbuf}
  (* Escapes *)
  | [^ '"' '\\']+
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      lex_string buf lexbuf
    }
  (* Unterminated string *)
  | eof { raise (SyntaxError ("lexer: unterminated string literal")) }

(* Note: Tsuki only supports single line comments *)
and lex_comment = parse
  | eol {new_line lexbuf; lex_token lexbuf}
  | eof {EOF}
  | _ {lex_comment lexbuf}
