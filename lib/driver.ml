open Batteries
open Frontend.Parse

(* The compiler pipeline *)
module Ast = Frontend.Ast
module Scope = Semant.Scope
module Tc = Semant.Typecheck
module Cgen = Codegen.Js

(* Barebones prelude that we include by default. This is a standin
   for an actual standard library. *)
let prelude =
  {|
function _print(s) {
  console.log(s);
}

function _tostring(a) {
  return a.toString();
}

function _charat(s, i) {
  return s.charCodeAt(i);
}

function _stringequal(s1, s2) {
  return s1 === s2;
}

function _stringcat(s1, s2) { 
  return s1 + s2;
}

function _listpush(a1, a2) { 
  let a3 = a1; a3.push(a2); return a3;
}

function _listindex(a1, i) {
  return a1[i];
}
|}

let prelude_types =
  [
    ("_print", Ast.TFunction ([ TString ], TAny));
    ("_tostring", Ast.TFunction ([ TAny ], TString));
    ("_charat", Ast.TFunction ([ TString; TInt ], TInt));
    ("_stringequal", Ast.TFunction ([ TString; TString ], TAny));
    ("_stringcat", Ast.TFunction ([ TString; TString ], TBool));
    ("_listpush", Ast.TFunction ([ TAny; TAny ], TAny));
    ("_listindex", Ast.TFunction ([ TAny; TInt ], TAny));
  ]

(* *)
let compile path = File.with_file_in path (fun f -> parse f)

let drive path =
  let result = compile path in
  match result with
  | Ok res ->
      let ctx =
        List.fold_left
          (fun m (k, v) -> Scope.add_symbol m k v)
          Scope.new_env prelude_types
      in
      (* run the typechecker on the program *)
      let _ = Tc.check_program ctx res in
      print_endline prelude;
      print_endline (Cgen.walk_ast res)
  | Error err -> print_endline ("error: " ^ err)
