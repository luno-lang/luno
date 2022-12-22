open Batteries
open Front.Parse
module Ast = Front.Ast

let compile path =
  let file = File.open_in path in
  parse file

let () =
  let result = compile "prelude/test.tsu" in
  match result with
  | Ok res -> print_endline (Ast.Pretty.string_of_program res)
  | Error err -> print_endline ("error: " ^ err)
