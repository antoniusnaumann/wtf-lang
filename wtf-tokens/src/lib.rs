use std::borrow::Cow;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Definition
    Func,
    Overload,
    Constructor,
    Record,
    Resource,
    Enum,
    Variant,

    // Declaration
    Let,
    Var,

    // Conditionals
    If,
    Else,
    Match,

    // Loops
    For,
    While,

    // Control Flow
    Return,
    Throw,
    Break,
    Continue,

    // Imports
    Use,
    Export,
    Package,

    // Literals
    True,
    False,
    None,

    // Unit Testing
    Test,
    Assert,

    // Identifiers and Literals
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    VersionLiteral(String),

    // Operators
    Plus,         // +
    Minus,        // -
    Asterisk,     // *
    Slash,        // /
    Equal,        // =
    DoubleEqual,  // ==
    NotEqual,     // !=
    GreaterThan,  // >
    LessThan,     // <
    GreaterEqual, // >=
    LessEqual,    // <=
    Arrow,        // ->
    DoubleArrow,  // =>
    QuestionMark, // ?
    SafeCall,     // ?.
    Bang,         // !
    Concat,       // ++
    // Remove,    // --
    Contains, // in

    And, // and
    Or,  // or
    Not, // not

    // Punctuation
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Semicolon,    // ;
    Colon,        // :
    Dot,          // .
    DoubleColon,  // ::
    At,           // @

    // Special tokens
    Newline,
    EmptyLine,
    Eof,
    Invalid(String),
}

impl Token {
    pub fn try_as_version_literal(&self) -> Cow<Token> {
        match self {
            Token::FloatLiteral(f) => {
                let token_str = f.to_string();
                // TODO: proper version string validation
                if !token_str.contains('f') && !token_str.contains('+') && !token_str.contains('-')
                {
                    Cow::Owned(Token::VersionLiteral(token_str.to_owned()))
                } else {
                    Cow::Borrowed(self)
                }
            }
            _ => Cow::Borrowed(self),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}
