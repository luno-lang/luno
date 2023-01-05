open Batteries
open Frontend.Ast

exception NotInScope of string

(* Our scope which contains a mapping between symbol names and types *)
module Env = Map.Make (String)

let new_env = Env.empty

let lookup_symbol (env : 'a Env.t) sym =
  try Env.find sym env with Not_found -> failwith "not in scope"

let has_symbol env sym =
  match Env.find_opt sym env with
  | Some _ -> true
  | None -> false

let add_symbol env sym val' = Env.add sym val' env
