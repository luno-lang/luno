use std::iter::Peekable;

use super::{
    ast::{Expr, Program, Stmt},
    lexer::{lex_tokens, Location, Token, TokenKind},
};

use crate::common::error::Error;
use crate::pass::types::Type;

/// Type which wraps our lexer iterator inside a `Peekable`
type PeekableLexer<'a> = Peekable<Box<dyn Iterator<Item = Token> + 'a>>;

type ParseResult<T> = Result<T, Error>;

pub struct Parser<'a> {
    src: &'a str,
    location: Location,

    // Track a list of imports we have collected from parsing the file
    pub imports: Vec<Stmt>,

    // Iterator over the tokens in the lexer
    pub lexer: PeekableLexer<'a>,
}

#[allow(dead_code)]
impl<'a> Parser<'a> {
    /// Create a new parser from the given source
    pub fn new(src: &'a str) -> Parser {
        Parser {
            src,
            location: 0..0,
            imports: Vec::new(),
            lexer: (Box::new(lex_tokens(src)) as Box<dyn Iterator<Item = Token>>).peekable(),
        }
    }

    fn eof(&self) -> Token {
        Token {
            kind: TokenKind::EOF,
            location: self.src.len() - 1..self.src.len(),
        }
    }

    pub fn peek(&mut self) -> Token {
        self.lexer.peek().cloned().unwrap_or(self.eof())
    }

    pub fn next(&mut self) -> Token {
        self.lexer.next().unwrap_or(self.eof())
    }

    pub fn expect(&mut self, token_kind: TokenKind) -> ParseResult<Token> {
        let token = self.peek();
        if token.kind == token_kind {
            self.next();
            self.location = token.clone().location;
            Ok(token)
        } else {
            Err(Error::UnexpectedToken {
                location: token.clone().location,
                got: token,
                expected: vec![token_kind],
            })
        }
    }

    pub fn eat(&mut self, tokens: &'static [TokenKind]) -> ParseResult<Token> {
        let token = self.peek();
        if tokens.contains(&token.kind) {
            self.next();
            self.location = token.clone().location;
            Ok(token)
        } else {
            Err(Error::UnexpectedToken {
                location: token.clone().location,
                got: token,
                expected: tokens.to_vec(),
            })
        }
    }

    //
    // Parser methods
    //

    fn parse_identifier(&mut self) -> ParseResult<String> {
        let token = self.eat(&[TokenKind::Ident])?;
        Ok(self.src[token.location].to_string())
    }

    fn parse_int(&mut self) -> ParseResult<Expr> {
        let token = self.expect(TokenKind::Integer)?;
        let literal = &self.src[token.location.clone()];

        Ok(Expr::Int(literal.parse::<i32>().unwrap()))
    }

    fn parse_string(&mut self) -> ParseResult<Expr> {
        let token = self.expect(TokenKind::String)?;
        let literal = &self.src[token.location.clone()];

        Ok(Expr::String(literal.to_string()))
    }

    pub fn parse_value(&mut self) -> ParseResult<Expr> {
        // Valid token kinds
        let value_token = vec![TokenKind::Integer, TokenKind::Ident, TokenKind::String];

        let token = self.peek();
        let literal = &self.src[token.location.clone()];

        match token.kind {
            TokenKind::Integer => self.parse_int(),
            TokenKind::String => self.parse_string(),
            TokenKind::LSquare => self.parse_array(),
            TokenKind::Ident => {
                self.next();
                let next_token = self.peek();
                return match next_token.kind {
                    TokenKind::LParen => {
                        let call = self.parse_call(literal.to_string());
                        self.next();
                        call
                    }
                    _ => Ok(Expr::Ident(literal.to_string())),
                };
            }
            _ => Err(Error::UnexpectedToken {
                location: token.location.clone(),
                got: token,
                expected: value_token,
            }),
        }
    }

    pub fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_value()?;
        while let Ok(op) = self.eat(&[TokenKind::Star, TokenKind::Slash]) {
            let right = self.parse_value()?;
            left = Expr::BinOp(op.kind, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_factor()?;
        while let Ok(op) = self.eat(&[TokenKind::Plus, TokenKind::Minus]) {
            let right = self.parse_term()?;
            left = Expr::BinOp(op.kind, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub fn parse_compare(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_term()?;
        let ops = &[
            TokenKind::Eq,
            TokenKind::NotEq,
            TokenKind::Lt,
            TokenKind::Gt,
            TokenKind::LtEq,
            TokenKind::GtEq,
        ];
        while let Ok(op) = self.eat(ops) {
            let right = self.parse_term()?;
            left = Expr::BinOp(op.kind, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_compare()
    }

    pub fn parse_array(&mut self) -> ParseResult<Expr> {
        self.expect(TokenKind::LSquare)?;
        let mut items = Vec::new();

        if self.peek().kind != TokenKind::RSquare {
            items.push(self.parse_expr()?);
        }

        while self.peek().kind != TokenKind::RSquare {
            self.expect(TokenKind::Comma)?;
            if self.peek().kind != TokenKind::RSquare {
                items.push(self.parse_expr()?);
            }
        }

        Ok(Expr::Array(items))
    }

    pub fn parse_call(&mut self, ident: String) -> ParseResult<Expr> {
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();

        if self.peek().kind != TokenKind::RParen {
            args.push(self.parse_expr()?);
        }

        while self.peek().kind != TokenKind::RParen {
            self.expect(TokenKind::Comma)?;
            if self.peek().kind != TokenKind::RParen {
                args.push(self.parse_expr()?);
            }
        }

        Ok(Expr::Call(ident, args))
    }

    // Parse a type name
    pub fn parse_type_name(&mut self) -> ParseResult<Type> {
        match self.parse_identifier()?.as_str() {
            "int" => Ok(Type::IntTy),
            "string" => Ok(Type::StringTy),
            "any" => Ok(Type::AnyTy),

            // Invalid type
            ty => Err(Error::InvalidTypeSpecified {
                location: self.location.clone(),
                got: ty.to_string(),
            }),
        }
    }

    //
    // Parsing statements
    //

    pub fn parse_stmt_expr(&mut self) -> ParseResult<Stmt> {
        // Wrap an expression inside a statement
        let inner = self.parse_expr()?;
        Ok(Stmt::Expr { inner })
    }

    pub fn parse_block(&mut self) -> ParseResult<Vec<Stmt>> {
        // Parse a block
        let mut stmts = Vec::new();

        while self.peek().kind != TokenKind::End {
            stmts.push(self.parse_stmt()?);
        }

        self.next();

        Ok(stmts)
    }

    pub fn parse_import(&mut self) -> ParseResult<Stmt> {
        // import "path_to_file"
        self.expect(TokenKind::Import)?;
        let path = self.parse_value()?;

        match path {
            Expr::String(s) => {
                let import = Stmt::Import { path: s };
                self.imports.push(import.clone());

                // Imports are special cased in the parser. When we encounter one,
                // we push it to the list of program imports
                Ok(import)
            }
            _ => Err(Error::MalformedImport {
                location: self.location.clone(),
            }),
        }
    }

    pub fn parse_var_declare(&mut self) -> ParseResult<Stmt> {
        //   var ident : ty = value
        // | var ident = value

        self.expect(TokenKind::Var)?;
        println!("peek: {:?}", self.lexer.peek());
        let ident = self.parse_identifier()?;

        match &self.next().kind {
            TokenKind::Colon => {
                let typ = self.parse_type_name()?;
                self.expect(TokenKind::Equal)?;
                let value = self.parse_value()?;

                Ok(Stmt::VarDeclare { typ, ident, value })
            }
            TokenKind::Equal => {
                let value = self.parse_value()?;
                Ok(Stmt::VarDeclare {
                    typ: Type::UnknownTy,
                    ident,
                    value,
                })
            }

            // We expected to see a ':' or '=' but we saw neither
            other => Err(Error::ExpectedType {
                location: self.location.clone(),
                got: other.clone(),
            }),
        }
    }

    pub fn parse_var_assign(&mut self) -> ParseResult<Stmt> {
        // ident = value
        let ident = self.parse_identifier()?;
        self.expect(TokenKind::Equal)?;
        let value = self.parse_value()?;

        Ok(Stmt::VarAssign { ident, value })
    }

    pub fn parse_type_annot(&mut self) -> ParseResult<(String, Type)> {
        let ident = self.parse_identifier()?;
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type_name()?;

        Ok((ident, typ))
    }

    // pub fn parse_if(&mut self) -> ParseResult<Stmt> {}

    pub fn parse_func_declare(&mut self) -> ParseResult<Stmt> {
        //    fn ident(p1: int, p2: int) do .. end
        self.expect(TokenKind::Fn)?;
        let ident = self.parse_identifier()?;

        // Parse fn params
        let mut params = Vec::new();
        let stmts: Vec<Stmt>;

        self.expect(TokenKind::LParen)?;
        if self.peek().kind != TokenKind::RParen {
            params.push(self.parse_type_annot()?);
        }

        while self.peek().kind != TokenKind::RParen {
            self.expect(TokenKind::Comma)?;
            if self.peek().kind != TokenKind::RParen {
                params.push(self.parse_type_annot()?);
            }
        }

        // Return type
        self.next();
        let ret_typ = self.parse_type_name()?;

        stmts = self.parse_block()?;

        Ok(Stmt::FnDeclare {
            ident,
            ret_typ,
            params,
            body: stmts,
        })
    }

    pub fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek().kind {
            TokenKind::Import => self.parse_import(),
            TokenKind::Ident => self.parse_var_assign(),
            TokenKind::Var => self.parse_var_declare(),
            TokenKind::Fn => self.parse_func_declare(),
            _ => self.parse_stmt_expr(),
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut stmts = Vec::new();

        println!("got: {:?}", self.peek().kind);

        while self.peek().kind != TokenKind::EOF {
            stmts.push(self.parse_stmt()?)
        }

        Ok(Program {
            imports: self.imports.clone(),
            stmts,
        })
    }
}
