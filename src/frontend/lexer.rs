use std::iter::Peekable;

use logos::Logos;

#[derive(Debug, Clone, PartialEq, Logos)]
pub enum TokenKind {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("=")]
    Equal,
    #[token("==")]
    Eq,
    #[token("~=")]
    NotEq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("=>")]
    FatArrow,

    // Keywords
    #[token("import")]
    Import,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("of")]
    Of,
    #[token("while")]
    While,
    #[token("then")]
    Then,
    #[token("var")]
    Var,
    #[token("let")]
    Let,
    #[token("end")]
    End,

    // Literals
    #[regex(r"[_a-zA-Z][_0-9a-zA-Z]*")]
    Ident,
    #[regex(r"(-)?[0-9]+")]
    Integer,
    #[regex(r#""(\\[\\"]|[^"])*""#)]
    String,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: std::ops::Range<usize>,

    // Use String so we don't have to have a lifetime everywhere we use
    // Token
    pub literal: String,
}

/// Return an iterator over the tokens in the lexer
pub fn lex_tokens<'a>(src: &'a str) -> impl Iterator<Item = Token> + 'a {
    TokenKind::lexer(src)
        .spanned()
        .map(|(k, s)| Token {
            kind: k,
            location: s.clone(),
            literal: src[s].to_string(),
        })
        .into_iter()
}

// Tests
#[cfg(test)]
mod tests {}
