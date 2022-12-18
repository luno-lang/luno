
%{
open Ast
%}

%token PLUS MINUS STAR SLASH LPAREN RPAREN
%token LBRACK RBRACK COMMA ASSIGN EQ NOT_EQ LT GT LT_EQ GT_EQ
%token FN IF ELSE FOR WHILE THEN VAR END
%token EOF

%token<int> INT
%token<string> STRING
%token<string> IDENT

%type<Ast.expr> expr
%type<Ast.stmt> stmt
// %type<Ast.program> program

/* Operator precedence (low to high) */
%left PLUS MINUS
%left STAR SLASH
%left LT GT LT_EQ GT_EQ

%start expr

%%
expr:
  | INT
    { Int ($startpos, $1) }
  | STRING
    { Str ($startpos, $1) }
  | IDENT 
    { Ident ($startpos, (Var $1)) }
  | e1 = expr; op = bin_op; e2 = expr
    { BinOp ($startpos, e1, op, e2) }
  
stmt:
  /* var x: int = 1 */
  | VAR; id=IDENT; ASSIGN; e=expr
    { VarDecl ($startpos, T_Num, id, e) }
  /* x = 1*/
  | id=IDENT; ASSIGN; e=expr
    { VarAssign ($startpos, id, e) }
  /* if (cond) then block_stmt else block_stmt */
  | IF; cond=expr; THEN; then_block=stmt; ELSE; else_block=stmt
    { If ($startpos, cond, then_block, else_block) }

%inline bin_op:
  | PLUS { Op_Plus }
  | MINUS { Op_Minus }
  | STAR { Op_Star }
  | SLASH { Op_Slash }
  | LPAREN { Op_LParen }
  | RPAREN { Op_RParen }
  | LBRACK { Op_LBrack }
  | RBRACK { Op_RBrack }
