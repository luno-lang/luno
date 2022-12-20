open Batteries
open Front.Parse

let compile path = 
  let file = File.open_in path in
  parse file

let () = 
  let result = compile "example/test.tsu" in
  match result with
  | Ok _ -> print_endline "ok"
  | Error err -> print_endline ("error: " ^ err)
  ;
  Front.Ast_print.test_printer