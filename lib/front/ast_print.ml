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
  let indent = pp.pre ^ String.repeat space pp.indent ^ "\n" in
  match exp with
  | Int (_,i) -> string_of_int i
  | Float (_, f) -> string_of_float f
  | Str (_, s) -> "\"" ^ s ^ "\""
  | Ident (_, Var(id)) -> id
  | BinOp (_, l, op, r) -> 
      let lhs = "left: " ^ expr_to_string pp l ^ "\n"
      and rhs = "right: " ^ expr_to_string pp r ^ "\n"
      and op = "op: " ^ bin_op_to_string op ^ "\n"
      in
        indent ^ lhs ^ " " ^ op ^ " " ^ rhs

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