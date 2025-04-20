use wtf_tokens::{Span, Token};

#[derive(Clone, Debug)]
pub struct Error {
    // pub source: PathBuf,
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    /// Identifier is not in scope
    UnknownIdentifier,

    /// Expected one of 'expected', got 'found'
    UnexpectedToken { expected: Vec<Token> },

    /// Expected one of 'expected', got 'found'
    ///
    /// 'found' can be an identifier that the user confused for a keyword
    UnexpectedKeyword { expected: Vec<Token> },
}
