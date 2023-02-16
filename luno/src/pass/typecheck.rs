use std::clone;

use crate::{
    common::error::Error,
    frontend::{
        ast::{Expr, Stmt},
        lexer::TokenKind,
    },
};
use im::HashMap;

use super::types::Type;

/// Type checker context.
///
/// Each time we enter a scope, we return a new immutable hashmap to prevent
/// leaking the scope outside.
#[derive(Clone)]
pub struct TypecheckerCtx {
    scope_env: HashMap<String, Type>,
}

type TypecheckResult<T> = Result<T, Error>;

impl TypecheckerCtx {
    fn lookup_symbol(&self, name: &String) -> Option<Type> {
        self.scope_env.get(name).cloned()
    }

    fn check_expr(&self, exp: Expr, typ: Type) -> TypecheckResult<TypecheckerCtx> {
        match (exp, typ) {
            (Expr::Ident(name), _) => {
                if self.lookup_symbol(&name).is_some() {
                    Ok(self.clone())
                } else {
                    Err(Error::NotInScope { name })
                }
            }
            (exp_, t) => {
                let synth_t = self.infer(exp_)?;

                if synth_t.is_compatible(t.clone()) {
                    Ok(self.clone())
                } else {
                    Err(Error::IncompatibleTypes { a: synth_t, b: t })
                }
            }
        }
    }

    fn check_stmt(&self, stmt: Stmt) -> TypecheckResult<TypecheckerCtx> {
        match stmt {
            Stmt::VarAssign { ident, value } => {
                let name_t = "";
                let value_t = self.infer(value);
                Ok(self.clone())
            }
            Stmt::If { cond, .. } => {
                let cond_t = self.infer(cond)?;
                if cond_t == Type::BoolTy || cond_t == Type::AnyTy {
                    Ok(self.clone())
                } else {
                    Err(Error::ExpectedBoolCondition)
                }
            }
            _ => todo!(),
        }
    }

    //
    // Inference
    //

    /// Walk the AST and perform type inference on each node
    pub fn infer(&self, exp: Expr) -> TypecheckResult<Type> {
        match exp {
            Expr::Boolean(_) => Ok(Type::BoolTy),
            Expr::Int(_) => Ok(Type::IntTy),
            Expr::Float(_) => Ok(Type::FloatTy),
            Expr::String(_) => Ok(Type::StringTy),
            Expr::Ident(name) => Ok(self.lookup_symbol(&name).unwrap()),

            Expr::BinOp(op, lhs, rhs) => self.infer_binop(op, *lhs, *rhs),
            _ => todo!(),
        }
    }

    fn infer_binop(&self, op: TokenKind, lhs: Expr, rhs: Expr) -> TypecheckResult<Type> {
        use crate::frontend::lexer::TokenKind as TK;

        let lhs_t = self.infer(lhs)?;
        let rhs_t = self.infer(rhs)?;
        match (&lhs_t, op, &rhs_t) {
            (Type::IntTy, TK::Plus | TK::Minus | TK::Star | TK::Slash, Type::IntTy) => {
                Ok(Type::IntTy)
            }
            (Type::StringTy, TK::Plus, Type::StringTy) => Ok(Type::StringTy),
            _ => Err(Error::TypeMismatch { a: lhs_t, b: rhs_t }),
        }
    }
}
