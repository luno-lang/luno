%{
open Ast
%}

%token PLUS MINUS STAR SLASH LPAREN RPAREN
%token LBRACK RBRACK COMMA COLON ASSIGN EQ NOT_EQ LT GT LT_EQ GT_EQ
%token FN IF ELSE FOR WHILE THEN VAR END
%token EOF

%token<int> INT
%token<string> STRING
%token<string> IDENT

%type<Ast.expr> expr
%type<Ast.stmt> stmt
%type<Ast.stmt> main

(* Operator precedence (low to high) *)
%left PLUS MINUS
%left STAR SLASH
%left EQ NOT_EQ
%left LT GT LT_EQ GT_EQ

%start main

%%

expr_l1:
  | expr STAR  expr {BinOp ($startpos, $1, Op_Star, $3)}
  | expr SLASH expr {BinOp ($startpos, $1, Op_Slash, $3)}
  | e = expr_l2 {e}

expr_l2:
  | expr PLUS  expr {BinOp ($startpos, $1, Op_Plus, $3)}
  | expr MINUS expr {BinOp ($startpos, $1, Op_Minus, $3)}
  | e = expr_l3 {e}

expr_l3:
  | expr LT    expr {BinOp ($startpos, $1, Op_Lt, $3)}
  | expr GT    expr {BinOp ($startpos, $1, Op_Gt, $3)}
  | expr LT_EQ expr {BinOp ($startpos, $1, Op_LtEq, $3)}
  | expr GT_EQ expr {BinOp ($startpos, $1, Op_GtEq, $3)}

expr_l4:
  | expr EQ     expr {BinOp ($startpos, $1, Op_Eq, $3)}
  | expr NOT_EQ expr {BinOp ($startpos, $1, Op_NotEq, $3)} 
  | e = expr_l5 {e}

(* Highest precedence level for an expression *)
expr_l5:
  (* ( ) have the highest precedence *)
  | LPAREN expr RPAREN {Group ($startpos, $2)}

expr:
  | INT {Int ($startpos, $1)}
  | STRING {Str ($startpos, $1)}
  | IDENT  {Ident ($startpos, (Var $1))}
  | lis = list {List ($startpos, lis)}
  | e = expr_l1 {e}

(* Lists *)
list_item:
  | e = expr {e}
list:
  | LBRACK; list_of = separated_list(COMMA, list_item); RBRACK {list_of}
  
(**
  This defines all the allowed statements in Tsuki. A valid statement can be one of:
  - a variable declaration
  - a variable reassignment
  - an if statement
  - a for loop
  - a function
*)
stmt:
  | VAR IDENT ASSIGN expr {VarDecl ($startpos, TDefault, $2, $4)}
  | IDENT ASSIGN expr {VarAssign ($startpos, $1, $3)}

(* Main entry point *)
main:
  stmt; EOF { $1 }
