(**
  Pretty print the untyped AST out (for debugging)
*)

open Batteries
open Ast

let space = " "

type printer_state = {
  indent: int;

  (* string to prepend *)
  pre: string; 
}

let rec show_expr (exp:expr) = 
  match exp with
  | Int (_, v) -> string_of_int v
  | Float (_, v) -> string_of_float v
  | Str (_, v) -> "\"" ^ v ^ "\""
  | Ident (_, v) -> v
  | Group (_, v) -> "(" ^ show_expr v ^ ")"
  | _ -> ""

let print_literal (ps:printer_state) exp = 
  let spaces = String.repeat " " ps.indent in
  print_string (spaces ^ ps.pre ^ show_expr exp)

let print_raw_string (ps:printer_state) str =
  let spaces = String.repeat " " ps.indent in
  print_string (spaces ^ ps.pre ^ str)

let rec print_expr (ps:printer_state) exp =
  (* Make print_list local to this function *)
  let print_list (ps:printer_state) exp =
    let spaces = String.repeat " " ps.indent in
    print_endline (spaces ^ ps.pre ^ "array");

    exp |> List.iter (fun it ->
      print_expr {pre = "item: "; indent = ps.indent + 2} it;
    print_newline ()) in

  match exp with
  | Int _ -> print_literal ps exp
  | Float _ -> print_literal ps exp
  | Str _ -> print_literal ps exp
  | Ident _ -> print_literal ps exp
  | Group _ -> print_literal ps exp
  | BinOp (_, l, op, r) -> print_binop ps l op r
  | List (_, xs) -> print_list ps xs

(* Messy code to pretty print a statement *)
and print_stmt (ps:printer_state) stmt =
  let spaces = String.repeat " " ps.indent in
  match stmt with
  | VarDecl (_, _, name, value) -> (
      print_endline (spaces ^ ps.pre ^ "var_decl");
      print_raw_string {pre = "name: "; indent = ps.indent + 2} name;
      print_newline ();

      print_expr {pre = "value: "; indent = ps.indent + 2} value;
      print_newline ()
  )
  | VarAssign (_, name, value) -> (
      print_endline (spaces ^ ps.pre ^ "var_assign");
      print_raw_string {pre = "name: "; indent = ps.indent + 2} name;
      print_newline ();
      
      print_expr {pre = "value: "; indent = ps.indent + 2} value;
      print_newline ()
  )
  | _ -> ()
  (* | If (_, cond, then_b, else_b) -> ()
  | ForOf (_, name, iter, block) -> ()
  | While (_, cond, block) -> () *)

and print_binop (ps:printer_state) l op r = 
  let spaces = String.repeat " " ps.indent in  
  print_endline (spaces ^ ps.pre ^ "operator " ^ string_of_op op);

  print_expr {pre = "left: "; indent = ps.indent + 2} l;
  print_newline ();
  print_expr {pre = "right: "; indent = ps.indent + 2} r;
  print_newline ()

(* Convert a bin_op to a string representation *)
and string_of_op (bop:bin_op) =
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

let test_printer = 
  (* Test the pretty printer out with a simple expression *)
  let p = Lexing.dummy_pos in
  let ps = { indent=0; pre="" } in
  let e = List(p, [Int(p, 1); Int(p, 2)]) in
  let s = VarAssign(p, "test", e) in
  print_stmt ps s