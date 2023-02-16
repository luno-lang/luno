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
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    // Keywords
    #[token("import")]
    Import,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("fn")]
    Fn,
    #[token("for")]
    For,
    #[token("do")]
    Do,
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

    EOF,
}

pub type Location = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

/// Return an iterator over the tokens in the lexer
pub fn lex_tokens<'a>(src: &'a str) -> impl Iterator<Item = Token> + 'a {
    TokenKind::lexer(src)
        .spanned()
        .map(|(k, s)| Token {
            kind: k,
            location: s.clone(),
        })
        .into_iter()
}
