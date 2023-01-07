use ariadne::{Report, ReportKind};
use thiserror::Error;

use crate::frontend::lexer::{Token, TokenKind};

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("expected {expected:?}, but got {:?}", got.kind)]
    UnexpectedToken { got: Token, expected: Vec<TokenKind> },

    #[error("reached EOF")]
    ReachedEof,
}

// TODO: Have a function which constructs a pretty diagnostic message
// given a parse error
impl ParseError {}
