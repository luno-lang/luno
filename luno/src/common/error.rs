use ariadne::{Report, ReportKind};
use thiserror::Error;

use crate::frontend::lexer::{Location, Token, TokenKind};

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("expected {expected:?}, but got {:?}", got.kind)]
    UnexpectedToken {
        location: Location,
        got: Token,
        expected: Vec<TokenKind>,
    },

    #[error("invalid type specified")]
    InvalidType { location: Location, got: String },

    #[error("malformed import statement")]
    MalformedImport { location: Location },

    #[error("expected a type, but got {:?}", got)]
    ExpectedType { location: Location, got: TokenKind },

    #[error("reached EOF")]
    ReachedEof,
}

// TODO: Have a function which constructs a pretty diagnostic message
// given a parse error
impl ParseError {
    pub fn produce_report(&self) -> Report {
        // match self {
        //     ParseError::ReachedEof => {}
        //     _ => {}
        // }
        todo!()
    }
}
