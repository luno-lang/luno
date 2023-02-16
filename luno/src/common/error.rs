use ariadne::Report;
use thiserror::Error;

use crate::{
    frontend::lexer::{Location, Token, TokenKind},
    pass::types::Type,
};

#[derive(Debug, PartialEq, Error)]
pub enum Error {
    #[error("Expected {expected:?}, but got {:?}", got.kind)]
    UnexpectedToken {
        location: Location,
        got: Token,
        expected: Vec<TokenKind>,
    },

    #[error("Invalid type specified")]
    InvalidTypeSpecified { location: Location, got: String },

    #[error("Malformed import statement")]
    MalformedImport { location: Location },

    #[error("Expected a type, but got {:?}", got)]
    ExpectedType { location: Location, got: TokenKind },

    #[error("Reached EOF")]
    ReachedEof,

    // Type checker errors
    #[error("Type mismatch between {a:?} and {b:?}")]
    TypeMismatch { a: Type, b: Type },

    #[error("Type {a:?} and {b:?} are not compatible")]
    IncompatibleTypes { a: Type, b: Type },

    #[error("Expected boolean condition")]
    ExpectedBoolCondition,

    #[error("{name} not in scope")]
    NotInScope { name: String },
}

// TODO: Have a function which constructs a pretty diagnostic message
// given a parse error
impl Error {
    pub fn produce_report(&self) -> Report {
        // match self {
        //     ParseError::ReachedEof => {}
        //     _ => {}
        // }
        todo!()
    }
}
