%{
open Token
open Ast
%}

%token PLUS
%token MINUS
%token STAR
%token SLASH
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token DOT
%token COMMA
%token COLON
%token ASSIGN
%token EQ
%token NOT_EQ
%token LT
%token GT
%token LT_EQ
%token GT_EQ
%token ARROW

(* Keywords *)
%token IMPORT
%token FOREIGN
%token FN 
%token IF
%token ELSE
%token FOR
%token OF
%token WHILE
%token THEN
%token VAR
%token CONST
%token END
%token EOF

(* Types *)
%token BOOL
%token INT
%token FLOAT
%token STRING

%token<bool> LIT_BOOL
%token<int> LIT_INT
%token<float> LIT_FLOAT
%token<string> LIT_STRING
%token<string> IDENT

%type<Ast.ty> ty
%type<Ast.expr> expr
%type<Ast.literal> literal
%type<Ast.stmt> stmt
%type<Ast.top_level> top_level
%type<Ast.program> program

(* Operator precedence (low to high) *)
%left PLUS MINUS
%left STAR SLASH
%left EQ NOT_EQ
%left LT GT LT_EQ GT_EQ
%left LPAREN RPAREN

%start program

%%
(* Type information *)
ty:
  | INT { TInt }
  | STRING { TString }

literal:
  | LIT_INT { AInt $1 }
  | LIT_STRING { AStr $1 }
expr:
  | exp=literal { Lit exp }
  | exp=IDENT { Ident exp }
  | exp=list_expr { List exp } 
  | e1=expr op=bin_op e2=expr { BinOp (e1, op, e2) }

list_item:
  | item=expr {item}
list_expr:
  | LBRACK items=separated_list(COMMA, list_item) RBRACK
  { items }

stmt:
  | VAR name=IDENT ASSIGN value=expr
  { VarDecl (TUntyped, name, value) }
  | name=IDENT ASSIGN value=expr
  { VarAssign (name, value) }
  (* TODO: make else_b optional *)
  | IF cond=expr THEN then_b=block ELSE else_b=block END
  { If (cond, then_b, else_b) }
  | FOR name=IDENT OF exp=expr THEN for_b=block END
  { For (name, exp, for_b) }
  | WHILE cond=expr THEN while_b=block END
  { While (cond, while_b) }

block:
  | list(stmt) { $1 }

func_definition:
  | FN name=IDENT LPAREN RPAREN body=block END
  { FuncDefn (name, body) }

(* Top level statements *)
top_level:
  | func_definition { $1 }

program:
  | list(top_level) EOF { Program $1 }

%inline bin_op:
  | PLUS { Op_Plus }
  | MINUS { Op_Minus }
  | STAR { Op_Star }
  | SLASH { Op_Slash }
  | EQ { Op_Eq }
  | NOT_EQ { Op_NotEq }
  | LT { Op_Lt }
  | GT { Op_Gt }
  | LT_EQ { Op_LtEq }
  | GT_EQ { Op_GtEq }
