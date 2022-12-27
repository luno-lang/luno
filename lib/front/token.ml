type token =
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | DOT
  | COMMA
  | COLON
  | ASSIGN
  | EQ
  | NOT_EQ
  | LT
  | GT
  | LT_EQ
  | GT_EQ
  | ARROW
  (* Keywords *)
  | IMPORT
  | FOREIGN
  | FN
  | IF
  | ELSE
  | FOR
  | OF
  | WHILE
  | THEN
  | VAR
  | CONST
  | END
  (* Types *)
  | BOOL
  | INT
  | FLOAT
  | STRING
  (* Literals *)
  | IDENT of string
  | LIT_BOOL of bool
  | LIT_INT of int
  | LIT_FLOAT of float
  | LIT_STRING of string
  | EOF

let rec string_of_tok t =
  match t with
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACK -> "["
  | RBRACK -> "]"
  | DOT -> "."
  | COMMA -> ","
  | COLON -> ":"
  | ASSIGN -> "="
  | EQ -> "=="
  | NOT_EQ -> "~="
  | LT -> "<"
  | GT -> ">"
  | LT_EQ -> "<="
  | GT_EQ -> ">="
  | ARROW -> "->"
  (* Keywords *)
  | IMPORT -> "import"
  | FOREIGN -> "foreign"
  | FN -> "fn"
  | IF -> "if"
  | ELSE -> "else"
  | FOR -> "for"
  | OF -> "of"
  | WHILE -> "while"
  | THEN -> "then"
  | VAR -> "var"
  | CONST -> "const"
  | END -> "end"
  | BOOL -> "bool"
  | INT -> "int"
  | FLOAT -> "float"
  | STRING -> "string"
  | IDENT s -> s
  | LIT_BOOL v -> string_of_bool v
  | LIT_INT v -> string_of_int v
  | LIT_FLOAT v -> string_of_float v
  | LIT_STRING v -> v
  | EOF -> "EOF"