use std::iter::Peekable;

use super::{
    ast::{Expr, LiteralType, Stmt, Type},
    lexer::{lex_tokens, Token, TokenKind},
};
use crate::common::error::ParseError;

/// Type which wraps our lexer iterator inside a `Peekable`
type PeekableLexer<'a> = Peekable<Box<dyn Iterator<Item = Token> + 'a>>;

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    src: &'a str,

    // Iterator over the tokens in the lexer
    pub lexer: PeekableLexer<'a>,
}

impl<'a> Parser<'a> {
    /// Create a new parser from the given source
    pub fn new(src: &'a str) -> Parser {
        Parser {
            src,
            lexer: (Box::new(lex_tokens(src)) as Box<dyn Iterator<Item = Token>>).peekable(),
        }
    }

    pub fn expect(&mut self, kind: TokenKind, msg: &'a str) -> ParseResult<Token> {
        let token = self.lexer.next().ok_or(ParseError::ReachedEof)?;

        if token.kind == kind {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken {
                got: token,
                expected: vec![kind],
            })
        }
    }

    pub fn consume(&mut self, tokens: &'static [TokenKind]) -> ParseResult<Token> {
        let token = self.lexer.peek().cloned().ok_or(ParseError::ReachedEof)?;
        if tokens.contains(&token.kind) {
            self.lexer.next().unwrap();
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken {
                got: token,
                expected: tokens.to_vec(),
            })
        }
    }

    // Expression parsing
    fn parse_value(&mut self) -> ParseResult<Expr> {
        let token = self.lexer.next().ok_or(ParseError::ReachedEof)?;
        match token.kind {
            TokenKind::Integer => Ok(Expr::Literal(LiteralType::Int(
                token.literal.parse::<i32>().unwrap(),
            ))),
            TokenKind::String => Ok(Expr::Literal(LiteralType::String(token.literal))),

            // Invalid token
            _ => Err(ParseError::UnexpectedToken {
                got: token,
                expected: vec![TokenKind::Integer, TokenKind::String],
            }),
        }
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_value()?;
        while let Ok(op) = self.consume(&[TokenKind::Star, TokenKind::Slash]) {
            let right = self.parse_value()?;
            left = Expr::BinOp(Box::new(left), op.kind, Box::new(right));
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_factor()?;
        while let Ok(op) = self.consume(&[TokenKind::Plus, TokenKind::Minus]) {
            let right = self.parse_term()?;
            left = Expr::BinOp(Box::new(left), op.kind, Box::new(right));
        }

        Ok(left)
    }

    fn parse_compare(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_term()?;
        let ops = &[TokenKind::Eq, TokenKind::NotEq, TokenKind::Lt, TokenKind::Gt, TokenKind::LtEq, TokenKind::GtEq];
        while let Ok(op) = self.consume(ops) {
            let right = self.parse_term()?;
            left = Expr::BinOp(Box::new(left), op.kind, Box::new(right));
        }

        Ok(left)
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_compare()
    }

    // Statements
    fn parse_var_declare(&mut self) -> ParseResult<Stmt> {
        self.expect(TokenKind::Var, "expected var keyword")?;
        let ident = self.parse_value()?;
        self.expect(TokenKind::Equal, "expected '='")?;
        let value = self.parse_value()?;

        Ok(Stmt::VarDecl(Type::AnyTy, "".into(), value))
    }
}

#[cfg(test)]
mod tests {
    use crate::frontend::lexer::TokenKind;

    use super::Parser;

    #[test]
    fn expect_failed() {
        let mut parser = Parser::new("1 + 2");

        assert_eq!(
            parser
                .expect(TokenKind::Integer, "expected an integer")
                .unwrap()
                .kind,
            TokenKind::Integer
        )
    }
}
