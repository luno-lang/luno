open Batteries
open Fe.Parse

(* The compiler pipeline *)
module Ast = Fe.Ast
module Tc = Me.Typecheck
module Cgen = Be.Codegen

(* Barebones prelude that we include by default. This is a standin
   for an actual standard library. *)
let prelude =
  {|
function JS_print(s) {
  console.log(s);
}

function JS_toString(a) {
  return a.toString();
}

function JS_stringEqual(s1, s2) {
  return s1 === s2;
}

function JS_stringConcat(s1, s2) { 
  return s1 + s2;
}

function JS_listPush(a1, a2) { 
  let a3 = a1; a3.push(a2); return a3;
}

function JS_listIndex(a1, i) {
  return a1[i];
}
|}

let prelude_types =
  [
    ("JS_print", Ast.TFunction ([ TString ], TAny));
    ("JS_toString", Ast.TFunction ([ TAny ], TString));
    ("JS_stringEqual", Ast.TFunction ([ TString; TString ], TAny));
    ("JS_stringConcat", Ast.TFunction ([ TString; TString ], TBool));
    ("JS_listPush", Ast.TFunction ([ TAny; TAny ], TAny));
    ("JS_listIndex", Ast.TFunction ([ TAny; TInt ], TAny));
  ]

let compile path = File.with_file_in path (fun f -> parse f)

let drive path =
  let result = compile path in
  match result with
  | Ok res ->
      let tc = Tc.new_context in
      let _ =
        List.iter (fun (name, ty) -> Hashtbl.add tc.vars name ty) prelude_types
      in
      let _ = Tc.check_program tc res in
      print_endline prelude;
      print_endline (Cgen.walk_ast res)
  | Error err -> print_endline ("error: " ^ err)
