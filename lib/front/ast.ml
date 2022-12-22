(**
  The untyped AST (pre type-checking)   
 *)

(* Position of the lexer *)
type loc = Lexing.position

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

type ty =
  | TInt
  | TFloat
  | TString
  | TIdent
  | TDefault

type literal =
  | AInt of int
  | AFloat of float
  | AStr of string

type expr =
  | Lit of literal
  | Ident of string
  | Lambda of (string list * expr)
  | BinOp of expr * bin_op * expr
  | Group of expr
  (* Lists *)
  | List of expr list

type stmt =
  | VarDecl of ty * string * expr (* var x: int = 1 *)
  | VarAssign of string * expr (* x = 1 *)
  | If of expr * stmt list * stmt list (* if cond then block else block end *)
  | For of string * expr * stmt list (* for x of items .. end *)
  | While of expr * stmt list (* while cond .. end *)

type top_level =
  | FuncDefn of string * stmt list
  | Block of stmt list

type program = Program of top_level

(* Pretty printing functions *)
module Pretty = struct
  let indent_string prefix n str = String.make n ' ' ^ prefix ^ str

  let string_of_literal = function
    | AInt d -> string_of_int d
    | AFloat f -> string_of_float f
    | AStr s -> "\"" ^ s ^ "\""

  let rec string_of_expr (prefix : string) (indent : int) expr =
    match expr with
    | Lit lit -> indent_string prefix indent (string_of_literal lit)
    | Ident v -> indent_string prefix indent v
    | BinOp (l, op, r) ->
        indent_string prefix indent ("operator " ^ string_of_op op ^ "\n")
        ^ string_of_expr "left:" (indent + 2) l
        ^ "\n"
        ^ string_of_expr "right:" (indent + 2) r
        ^ "\n"
    | Group v ->
        indent_string prefix indent "group\n"
        ^ string_of_expr "value:" (indent + 2) v
    | List xs ->
        indent_string prefix indent "list\n"
        ^ String.concat "\n"
            (List.map (fun a -> string_of_expr "◦" (indent + 2) a) xs)
    | _ -> ""

  and string_of_stmt (prefix : string) (indent : int) stmt =
    match stmt with
    | VarDecl (_, name, exp) ->
        indent_string prefix indent "var_decl\n"
        ^ indent_string "name:" (indent + 2) name
        ^ "\n"
        ^ string_of_expr "value:" (indent + 2) exp
        ^ "\n"
    | VarAssign (name, exp) ->
        indent_string prefix indent "var_assign\n"
        ^ indent_string "name:" (indent + 2) name
        ^ "\n"
        ^ string_of_expr "value:" (indent + 2) exp
        ^ "\n"
    | _ -> ""

  and string_of_top_level (prefix : string) (indent : int) tl =
    match tl with
    | FuncDefn (name, stmts) ->
        indent_string prefix indent "func_defn\n"
        ^ indent_string "name:" (indent + 2) name
        ^ "\n"
        ^ String.concat "\n"
            (List.map (fun a -> string_of_stmt "" (indent + 2) a) stmts)
    | Block stmts ->
        indent_string prefix indent "block\n"
        ^ String.concat "\n"
            (List.map (fun a -> string_of_stmt "" (indent + 2) a) stmts)

  and string_of_program (Program tl) = string_of_top_level "" 0 tl

  and string_of_op (bop : bin_op) =
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
    | Op_Eq -> "=="
    | Op_NotEq -> "~="
    | Op_Lt -> "<"
    | Op_LtEq -> "<="
    | Op_Gt -> ">"
    | Op_GtEq -> ">="
end
