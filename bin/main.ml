open Batteries
open Fe.Parse
module Ast = Fe.Ast
module Check = Pass.Typecheck
module Gen = Be.Codegen

let compile path =
  let file = File.open_in path in
  parse file

let () =
  let result = compile "stdlib/string.tsu" in
  match result with
  | Ok res ->
      let ctx = Hashtbl.create 20 in
      Check.check_program ctx res;
      print_endline (Gen.walk_ast res);
      ()
  | Error err -> print_endline ("error: " ^ err)
