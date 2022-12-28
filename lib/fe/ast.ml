type loc = Lexing.position

(* Binary operators *)
type bin_op =
  | OPlus
  | OMinus
  | OStar
  | OSlash
  | OLParen
  | ORParen
  | OLBrack
  | ORBrack
  | OComma
  | OColon
  | OAssign
  | OEq
  | ONe
  | OLt
  | OGt
  | OLtEq
  | OGtEq

type ty =
  | TInt
  | TFloat
  | TString
  | TBool
  | TAny
  | TNeedsInfer

type literal =
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of string

type expr =
  | Lit of literal
  | Ident of string
  | FuncCall of string * expr list
  | BinOp of expr * bin_op * expr
  (* Lists *)
  | List of expr list

type stmt =
  | ShortVarDecl of string * expr (* x := 1 *)
  | VarDecl of ty * string * expr (* var x: int = 1 *)
  | VarAssign of string * expr (* x = 1 *)
  | If of expr * block * block option (* if cond then block else block end *)
  | For of string * expr * block (* for x of items .. end *)
  | While of expr * block (* while cond .. end *)
  | FuncCall of expr

and block = Block of stmt list

type top_level =
  | Import of string
  | FuncDefn of string * (string * ty) list * block
  | Stmt of stmt

type program = Program of top_level list

(* Pretty printing functions *)
module Pretty = struct
  let indent_string prefix n str = String.make n ' ' ^ prefix ^ str

  let string_of_literal = function
    | LitInt d -> string_of_int d
    | LitFloat f -> string_of_float f
    | LitStr s -> "\"" ^ s ^ "\""

  let rec string_of_expr (prefix : string) (indent : int) expr =
    match expr with
    | Lit lit -> indent_string prefix indent (string_of_literal lit)
    | Ident name -> indent_string prefix indent name
    | BinOp (l, op, r) ->
        indent_string prefix indent ("operator " ^ string_of_op op ^ "\n")
        ^ string_of_expr "left:" (indent + 2) l
        ^ "\n"
        ^ string_of_expr "right:" (indent + 2) r
        ^ "\n"
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
    | Import path -> "import " ^ path
    | FuncDefn (name, params, block) ->
        indent_string prefix indent "func_defn\n"
        ^ indent_string "name:" (indent + 2) name
        ^ "\n"
        ^ string_of_block (indent + 2) block
    | _ -> ""

  and string_of_block (indent : int) = function
    | Block bl ->
        indent_string "" indent "block\n"
        ^ String.concat "\n"
            (List.map (fun a -> string_of_stmt "" (indent + 2) a) bl)

  and string_of_program (Program tl) =
    String.concat "\n" (List.map (fun a -> string_of_top_level "" 0 a) tl)

  and string_of_op (bop : bin_op) =
    match bop with
    | OPlus -> "+"
    | OMinus -> "-"
    | OStar -> "*"
    | OSlash -> "/"
    | OLParen -> "("
    | ORParen -> ")"
    | OLBrack -> "["
    | ORBrack -> "]"
    | OComma -> ","
    | OColon -> ":"
    | OAssign -> "="
    | OEq -> "=="
    | ONe -> "~="
    | OLt -> "<"
    | OGt -> ">"
    | OLtEq -> "<="
    | OGtEq -> ">="
end
