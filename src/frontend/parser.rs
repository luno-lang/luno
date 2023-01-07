use std::iter::Peekable;

use super::{
    error::ParseError,
    lexer::{lex_tokens, Token, TokenKind},
};

/// Type which wraps our lexer iterator inside a `Peekable`
type PeekableLexer<'a> = Peekable<Box<dyn Iterator<Item = Token> + 'a>>;

pub struct Parser<'a> {
    src: &'a str,

    // The current token we are parsing currently
    token: Option<Token>,

    // Iterator over the tokens in the lexer
    pub lexer: PeekableLexer<'a>,
}

impl<'a> Parser<'a> {
    /// Create a new parser from the given source
    pub fn new(src: &'a str) -> Parser {
        Parser {
            src: src,
            token: None,
            lexer: (Box::new(lex_tokens(src)) as Box<dyn Iterator<Item = Token>>).peekable(),
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.token = self.lexer.peek().cloned();
        self.token.clone()
    }

    pub fn advance(&mut self) -> Option<Token> {
        self.token = self.lexer.next();
        self.token.clone()
    }

    /// Attempt to consume a token `kind` and return an error if we cannot
    pub fn expect(&mut self, kind: TokenKind, msg: &'a str) -> Result<Token, ParseError> {
        let token = self.advance().ok_or(ParseError::ReachedEof)?;

        if token.kind == kind {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken {
                token: token,
                expected: kind,
            })
        }
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
