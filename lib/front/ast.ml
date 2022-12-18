(**
 * The AST (pre type-checking)   
 *)

(* Position of the lexer *)
type loc = Lexing.position

type ident = Var of string

(* Binary operators *)
type bin_op =
  | Op_Plus
  | Op_Minus
  | Op_Star
  | Op_Slash
  | Op_LParen
  | Op_RParen
  | Op_LBrack
  | Op_RBrack
  | Op_Comma
  | Op_Colon
  | Op_Assign
  | Op_Eq
  | Op_NotEq
  | Op_Lt
  | Op_Gt
  | Op_LtEq
  | Op_GtEq

and ty =
  | TNum
  | TString

and expr =
  | Int of loc * int
  | Float of loc * float
  | Str of loc * string
  | Ident of loc * ident
  | BinOp of loc * expr * bin_op * expr

and stmt =
  (* var x: int = 1 *)
  | VarDecl of loc * ty * expr * expr
  (* x = 1 *)
  | VarAssign of loc * expr * expr
  (* if (cond) then block_stmt else block_stmt *)
  | If of loc * expr * block_stmt * block_stmt

and block_stmt = Block of stmt list

and program = Program of stmt list