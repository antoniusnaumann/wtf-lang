use std::{borrow::Cow, fmt::Display};

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
    /// Useful where only the category of the token is relevant but the payload does not matter
    ///
    /// e.g., when printing a list of expected tokens
    pub fn category_name(&self) -> &'static str {
        match self {
            Token::Func => "'func'",
            Token::Overload => "'overload'",
            Token::Constructor => "'constructor'",
            Token::Record => "'record'",
            Token::Resource => "'resource'",
            Token::Enum => "'enum'",
            Token::Variant => "'variant'",
            Token::Let => "'let'",
            Token::Var => "'var'",
            Token::If => "'if'",
            Token::Else => "'else'",
            Token::Match => "'match'",
            Token::For => "'for'",
            Token::While => "'while'",
            Token::Return => "'return'",
            Token::Throw => "'throw'",
            Token::Break => "'break'",
            Token::Continue => "'continue'",
            Token::Use => "'use'",
            Token::Export => "'export'",
            Token::Package => "'package'",
            Token::True => "'true'",
            Token::False => "'false'",
            Token::None => "'none'",
            Token::Test => "'test'",
            Token::Assert => "'assert'",
            Token::Identifier(_) => "an identifier",
            Token::IntegerLiteral(_) => "an integer",
            Token::FloatLiteral(_) => "a float",
            Token::StringLiteral(_) => "a string",
            Token::VersionLiteral(_) => "a package version",
            Token::Plus => "'+'",
            Token::Minus => "'-'",
            Token::Asterisk => "'*'",
            Token::Slash => "'/'",
            Token::Equal => "'='",
            Token::DoubleEqual => "'=='",
            Token::NotEqual => "'!='",
            Token::GreaterThan => "'>'",
            Token::LessThan => "'<'",
            Token::GreaterEqual => "'>='",
            Token::LessEqual => "'<='",
            Token::Arrow => "'->'",
            Token::DoubleArrow => "'=>'",
            Token::QuestionMark => "'?'",
            Token::SafeCall => "'?.'",
            Token::Bang => "'!'",
            Token::Concat => "'++'",
            Token::Contains => "'in'",
            Token::And => "'and'",
            Token::Or => "'or'",
            Token::Not => "'not'",
            Token::LeftParen => "'('",
            Token::RightParen => "')'",
            Token::LeftBrace => "'{'",
            Token::RightBrace => "'}'",
            Token::LeftBracket => "'['",
            Token::RightBracket => "']'",
            Token::Comma => "','",
            Token::Semicolon => "';'",
            Token::Colon => "':'",
            Token::Dot => "'.'",
            Token::DoubleColon => "'::'",
            Token::At => "'@'",
            Token::Newline => "'\\n'",
            Token::EmptyLine => "an empty line",
            Token::Eof => "end of file",
            Token::Invalid(_) => "an invalid token",
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Func => "func",
                Token::Overload => "overload",
                Token::Constructor => "constructor",
                Token::Record => "record",
                Token::Resource => "resource",
                Token::Enum => "enum",
                Token::Variant => "variant",
                Token::Let => "let",
                Token::Var => "var",
                Token::If => "if",
                Token::Else => "else",
                Token::Match => "match",
                Token::For => "for",
                Token::While => "while",
                Token::Return => "return",
                Token::Throw => "throw",
                Token::Break => "break",
                Token::Continue => "continue",
                Token::Use => "use",
                Token::Export => "export",
                Token::Package => "package",
                Token::True => "true",
                Token::False => "false",
                Token::None => "none",
                Token::Test => "test",
                Token::Assert => "assert",
                Token::Plus => "+",
                Token::Minus => "-",
                Token::Asterisk => "*",
                Token::Slash => "/",
                Token::Equal => "=",
                Token::DoubleEqual => "==",
                Token::NotEqual => "!=",
                Token::GreaterThan => ">",
                Token::LessThan => "<",
                Token::GreaterEqual => ">=",
                Token::LessEqual => "<=",
                Token::Arrow => "->",
                Token::DoubleArrow => "=>",
                Token::QuestionMark => "?",
                Token::SafeCall => "?.",
                Token::Bang => "!",
                Token::Concat => "++",
                Token::Contains => "in",
                Token::And => "and",
                Token::Or => "or",
                Token::Not => "not",
                Token::LeftParen => "(",
                Token::RightParen => ")",
                Token::LeftBrace => "{",
                Token::RightBrace => "}",
                Token::LeftBracket => "[",
                Token::RightBracket => "]",
                Token::Comma => ",",
                Token::Semicolon => ";",
                Token::Colon => ":",
                Token::Dot => ".",
                Token::DoubleColon => "::",
                Token::At => "@",
                Token::Newline => "\\n",
                Token::EmptyLine => "EMPTY LINE",
                Token::Eof => "EOF",

                Token::Identifier(ident) => return write!(f, "{ident}"),
                Token::IntegerLiteral(int) => return write!(f, "{int}"),
                Token::FloatLiteral(float) => return write!(f, "{float}"),
                Token::StringLiteral(string) => return write!(f, "{string}"),
                Token::VersionLiteral(version) => return write!(f, "{version}"),
                Token::Invalid(invalid) => return write!(f, "invalid: {invalid}"),
            }
        )
    }
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

impl Span {
    pub fn to(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}
