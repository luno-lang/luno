(**
 * Pretty print the untyped AST out (for debugging)
*)

open Batteries
open Ast

let space = " "

type printer_state = {
  indent: int;

  (* string to prepend *)
  pre: string; 
}

let create_printer : printer_state = 
  {
    indent = -2;
    pre = ""
  }

(* TODO: fix indentation *)
let rec expr_to_string (pp:printer_state) (exp:expr) =
  (* let indent = pp.pre ^ String.repeat space pp.indent ^ "\n"  *)
  let _ = pp.pre ^ String.repeat space pp.indent ^ "\n" in
  match exp with
  | Int (_,i) -> string_of_int i
  | Float (_, f) -> string_of_float f
  | Str (_, s) -> "\"" ^ s ^ "\""
  | Ident (_, Var(id)) -> id
  | BinOp (_, l, op, r) -> expr_to_string pp l ^ bin_op_to_string op ^ expr_to_string pp r
  | Group (_,e) -> "(" ^ expr_to_string pp e ^ ")"

  (* TODO: add support for printing lists *)
  | List (_, _) -> ""

and bin_op_to_string (bop:bin_op) =
  match bop with
  | Op_Plus -> "+"
  | Op_Minus -> "-"
  | Op_Star -> "*"
  | Op_Slash -> "/"
  | Op_LParen -> "("
  | Op_RParen -> ")"
  | Op_LBrack -> "["
  | Op_RBrack -> "]"
  | Op_Comma -> ","
  | Op_Colon -> ":"
  | Op_Assign -> "="
  | Op_Eq -> "==" | Op_NotEq -> "~="
  | Op_Lt -> "<" | Op_LtEq -> "<="
  | Op_Gt -> ">" | Op_GtEq -> ">="

and print_expr (pp:printer_state) (exp:expr) =
  expr_to_string { pp with indent = pp.indent + 4 } exp