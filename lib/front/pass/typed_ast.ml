(**
  The AST annotated with type information
 *)

module Ast = Front.Ast

type ty = Ast.ty
type loc = Lexing.position

(* An expression annotated with its type *)
type expr =
  | Lit of ty * Ast.literal
  | Ident of ty * string
  | BinOp of ty * expr * Ast.bin_op * expr

type stmt =
  | VarDecl of ty * string * expr
  | VarAssign of ty * string * expr
  | If of ty * expr * stmt list * stmt list
  | For of string * expr * stmt list
  | While of expr * stmt list
