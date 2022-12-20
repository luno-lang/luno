(**
  The AST (pre type-checking)   
 *)

(* Position of the lexer *)
type loc = Lexing.position
type ident = string

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
  | TDefault

and expr =
  | Int of loc * int
  | Float of loc * float
  | Str of loc * string
  | Ident of loc * ident
  | BinOp of loc * expr * bin_op * expr
  | Group of loc * expr

  (* Lists *)
  | List of loc * expr list

and stmt =
  | VarDecl of loc * ty * ident * expr   (* var x: int = 1 *)
  | VarAssign of loc * ident * expr      (* x = 1 *)
  | If of loc * expr * stmt * stmt       (* if cond then block else block end *)
  | ForOf of loc * ident * expr * stmt   (* for x of items .. end *)
  | While of loc * expr * stmt           (* while cond .. end *)

and top_level =
  | FuncDefn of loc * ident * stmt list
  | Block of stmt list

and program = Program of top_level