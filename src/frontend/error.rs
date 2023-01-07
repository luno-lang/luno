use ariadne::{Report, ReportKind};
use thiserror::Error;

use super::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("expected {expected:?}, but got {:?}", token.kind)]
    UnexpectedToken { token: Token, expected: TokenKind },

    #[error("reached EOF")]
    ReachedEof,
}

// TODO: Have a function which constructs a pretty diagnostic message
// given a parse error
impl ParseError {}
