open Batteries
open Fe.Parse

(* NOTE: it would be a waste to Open all these.. *)
module Ast = Fe.Ast
module Check = Pass.Typecheck
module Gen = Be.Codegen

let compile path =
  let file = File.open_in path in
  parse file

(* Include the prelude *)
let include_prelude =
  {|
// Barebones JS prelude until we have an actual stdlib 
function print(s) { console.log(s); }
function strcmp(s1, s2) { return (s1 === s2); }
function len(a) { return parseInt(a.length); }
function concat(s1, s2) { return s1 + s2; }
function append(a1, a2) { return a1.concat(a2); }
function split(s1, delim) { return s1.split(delim); }
function at_index(xs, i) { return xs[i]; }
function to_string(a) { return a.toString(); }

function and(a1, a2) { return (a1 && a2); }
function or(a1, a2) { return (a1 || a2); }
function bool_to_str(a) { return a === true ? "true" : "false"; }
|}

let () =
  let result = compile "examples/hello.tsu" in
  match result with
  | Ok res ->
      let ctx = Check.new_context in
      let _ = Check.check_program ctx res in
      print_endline include_prelude;
      print_endline (Gen.walk_ast res);

      ()
  | Error err -> print_endline ("error: " ^ err)
