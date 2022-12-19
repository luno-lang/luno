open Batteries
open Lexer
open Lexing

(* Print the location where an error occured to a string *)
let print_err_pos lexbuf = 
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_program lexbuf =
  try Ok (Parser.main Lexer.lex_token lexbuf) with
  | SyntaxError msg ->
    let error_msg = Printf.sprintf "%s: %s" (print_err_pos lexbuf) msg in
    Error error_msg
  (* grammar error *)
  | Parser.Error ->
    let error_msg = Printf.sprintf "syntax error on line: %s" (print_err_pos lexbuf) in
    Error error_msg

let parse ch  =
  let lexbuf = Lexing.from_channel ch in
  parse_program lexbuf
