use super::lexer::TokenKind;

pub enum Type {
    IntTy,
    FloatTy,
    StringTy,
    BoolTy,
    FunctionTy(Vec<Type>, Box<Type>),
    AnyTy,
}

pub enum LiteralType {
    Boolean(bool),
    Int(i32),
    Float(f32),
    String(String),
}

pub enum Expr {
    Literal(LiteralType),
    BinOp(Box<Expr>, TokenKind, Box<Expr>),
    Ident(String),
    Call(String, Vec<Expr>),
    List(Vec<Expr>),
}

pub enum Stmt {
    VarDecl(Type, String, Expr),
    VarAssign(Type, Expr),
    Expr(Expr),
}

pub enum TopLevelStmt {
    Import(String),
}
