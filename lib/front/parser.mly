%{
open Ast
%}

%token PLUS MINUS STAR SLASH LPAREN RPAREN
%token LBRACK RBRACK COMMA COLON ASSIGN EQ NOT_EQ LT GT LT_EQ GT_EQ
%token FN IF ELSE FOR OF WHILE THEN VAR END
%token EOF

%token<int> INT
%token<string> STRING
%token<string> IDENT

%type<Ast.expr> expr
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

bin_expr:
  | LPAREN e=expr RPAREN {Group ($startpos, e)}
  | e1=expr op=bin_op e2=expr {BinOp ($startpos, e1, op, e2)}

(* An expression can be a literal, list or function call *)
expr:
  | value=INT {Int ($startpos, value)}
  | value=STRING {Str ($startpos, value)}
  | value=IDENT {Ident ($startpos, value)}
  | value=list_value {List ($startpos, value)}
  | value=bin_expr {value}

(* Lists *)
list_item:
  | item=expr {item}
list_value:
  | LBRACK items=separated_list(COMMA, list_item) RBRACK 
    {items}

(**
  This defines all the allowed statements in Tsuki. A valid statement can be one of:
  - a variable declaration
  - a variable reassignment
  - an if statement
  - a for loop
  - a function
*)
stmt:
  | VAR name=IDENT ASSIGN value=expr 
    {VarDecl ($startpos, TDefault, name, value)}
  | name=IDENT ASSIGN value=expr 
    {VarAssign ($startpos, name, value)}
  | IF cond=expr THEN then_b=stmt ELSE else_b=stmt END
    {If ($startpos, cond, then_b, else_b)}
  | FOR name=IDENT OF list=expr for_b=stmt END
    {ForOf ($startpos, name, list, for_b)}
  | WHILE cond=expr while_b=stmt END
    {While ($startpos, cond, while_b)}

(* Top level statements *)
block:
  | b=list(stmt) {b}

top_level:
  | b=block {Block b}
  | FN name=IDENT fn_b=block END {FuncDefn ($startpos, name, fn_b)}

program:
  | tl=top_level; EOF { Program tl }

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
