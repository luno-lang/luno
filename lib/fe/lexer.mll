{
open Batteries
open Lexing
open Token

exception SyntaxError of string

let kw_table : (string, token) Hashtbl.t = Hashtbl.create 20
let reserved_kw_table : (string, token) Hashtbl.t = Hashtbl.create 20

let _ = 
  List.iter (fun (kw, ty) -> Hashtbl.add kw_table kw ty)
    [ ("import", IMPORT);
      ("fn", FN);
      ("if", IF);
      ("else", ELSE);
      ("for", FOR);
      ("of", OF);
      ("while", WHILE);
      ("then", THEN);
      ("var", VAR);
      ("let", LET);
      ("ret", RET);
      ("end", END);
      ("true", LIT_BOOL true);
      ("false", LIT_BOOL false);

      (* Types *)
      ("any", ANY);
      ("bool", BOOL);
      ("int", INT);
      ("float", FLOAT);
      ("string", STRING); ]

let _ = 
  List.iter (fun (kw, ty) -> Hashtbl.add reserved_kw_table kw ty)
  [ ]
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
  | "=>" {ARROW}
  | '"'  {lex_string (Buffer.create 16) lexbuf}
  | "#" {lex_comment lexbuf}

  | ident as s
    {
      match Hashtbl.find_option kw_table s with
      | Some kw -> kw
      | None -> 
        if Hashtbl.mem reserved_kw_table s
        then failwith "lexer: reserved keyword (for the future)"
        else IDENT s
    }
  | int as i {LIT_INT (int_of_string i)}
  (* TODO: add float *)
  | whitespace {lex_token lexbuf}
  | eol {new_line lexbuf; lex_token lexbuf}
  | eof {EOF}
  | _ { raise (SyntaxError ("lexer: illegal character " ^ Lexing.lexeme lexbuf)) }

and lex_string buf = parse
  | '"' {LIT_STRING (Buffer.contents buf)}
  | '\\' '\n' {Buffer.add_char buf '\n'; lex_string buf lexbuf}
  (* Escapes *)
  | [^ '"' '\\']+
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      lex_string buf lexbuf
    }
  (* Unterminated string *)
  | eof { raise (SyntaxError ("lexer: unterminated string literal")) }

(* Note: Luno only supports single line comments *)
and lex_comment = parse
  | eol {new_line lexbuf; lex_token lexbuf}
  | eof {EOF}
  | _ {lex_comment lexbuf}
