use super::lexer::TokenKind;
use crate::pass::types::Type;

#[derive(Debug, Clone)]
pub enum Expr {
    Boolean(bool),
    Int(i32),
    Float(f32),
    String(String),

    Array(Vec<Expr>),
    BinOp(TokenKind, Box<Expr>, Box<Expr>),
    Ident(String),
    Call(String, Vec<Expr>),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Import {
        path: String,
    },
    FnDeclare {
        ident: String,
        ret_typ: Type,
        params: Vec<(String, Type)>,
        body: Vec<Stmt>,
    },
    VarDeclare {
        typ: Type,
        ident: String,
        value: Expr,
    },
    VarAssign {
        ident: String,
        value: Expr,
    },
    If {
        cond: Expr,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
    Expr {
        inner: Expr,
    },
}

#[derive(Debug, Clone)]
pub struct Program {
    /// List of imports we have collected during parsing
    pub imports: Vec<Stmt>,

    pub stmts: Vec<Stmt>,
}
