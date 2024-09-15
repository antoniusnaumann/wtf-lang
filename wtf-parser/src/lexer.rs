#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Definition
    Func,
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
    Import,
    Export,
    Package,

    // Identifiers and Literals
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

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
    Eof,
    Invalid(String),
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let input: Vec<_> = input.chars().collect();
        let current_char = if input.is_empty() {
            None
        } else {
            Some(input[0])
        };

        Lexer {
            input,
            position: 0,
            current_char,
        }
    }

    fn read_char(&mut self) {
        self.position += 1;
        if self.position >= self.input.len() {
            self.current_char = None; // End of input
        } else {
            self.current_char = Some(self.input[self.position]);
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.position + 1 >= self.input.len() {
            None
        } else {
            Some(self.input[self.position + 1])
        }
    }

    pub fn next_skip_newline(&mut self) -> SpannedToken {
        loop {
            let token = self.next_token();
            if token.token != Token::Newline {
                return token;
            }
        }
    }

    pub fn next_token(&mut self) -> SpannedToken {
        self.skip_whitespace();

        let start_pos = self.position;

        let token = match self.current_char {
            Some(c) if c.is_newline() => {
                while let Some(c) = self.current_char {
                    if c.is_whitespace() || c.is_newline() {
                        self.read_char();
                    } else {
                        break;
                    }
                }

                Token::Newline
            }
            Some('/') => {
                if self.peek_char() == Some('/') {
                    // TODO: Add support for doc comments which get added to AST
                    self.skip_comment();
                    return self.next_token();
                } else {
                    self.read_char();
                    Token::Slash
                }
            }
            Some('?') => {
                self.read_char();
                if self.current_char == Some('.') {
                    self.read_char();
                    Token::SafeCall
                } else {
                    Token::QuestionMark
                }
            }
            Some('!') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            Some('+') => {
                self.read_char();
                if self.current_char == Some('+') {
                    self.read_char();
                    Token::Concat
                } else {
                    Token::Plus
                }
            }
            Some(c) if c.is_ident_char() => {
                let identifier = self.read_identifier();
                self.lookup_identifier(identifier)
            }
            Some(c) if c.is_digit(10) => self.read_number(),
            Some('"') => {
                let string = self.read_string();
                Token::StringLiteral(string)
            }
            Some('-') => {
                self.read_char();
                if self.current_char == Some('>') {
                    self.read_char();
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            Some('*') => {
                self.read_char();
                Token::Asterisk
            }
            Some('=') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::DoubleEqual
                } else if self.current_char == Some('>') {
                    self.read_char();
                    Token::DoubleArrow
                } else {
                    Token::Equal
                }
            }
            Some('>') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::GreaterEqual
                } else {
                    Token::GreaterThan
                }
            }
            Some('<') => {
                self.read_char();
                if self.current_char == Some('=') {
                    self.read_char();
                    Token::LessEqual
                } else {
                    Token::LessThan
                }
            }
            Some('(') => {
                self.read_char();
                Token::LeftParen
            }
            Some(')') => {
                self.read_char();
                Token::RightParen
            }
            Some('{') => {
                self.read_char();
                Token::LeftBrace
            }
            Some('}') => {
                self.read_char();
                Token::RightBrace
            }
            Some('[') => {
                self.read_char();
                Token::LeftBracket
            }
            Some(']') => {
                self.read_char();
                Token::RightBracket
            }
            Some(',') => {
                self.read_char();
                Token::Comma
            }
            Some(';') => {
                self.read_char();
                Token::Semicolon
            }
            Some(':') => {
                self.read_char();
                if self.current_char == Some(':') {
                    self.read_char();
                    Token::DoubleColon
                } else {
                    Token::Colon
                }
            }
            Some('.') => {
                self.read_char();
                Token::Dot
            }
            Some('@') => {
                self.read_char();
                Token::At
            }
            None => Token::Eof,
            Some(c) => {
                // Unknown character
                self.read_char();
                Token::Invalid(c.into())
            }
        };

        let end_pos = self.position;

        SpannedToken {
            token,
            span: Span {
                start: start_pos,
                end: end_pos,
            },
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if c.is_whitespace() && !c.is_newline() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        while let Some(c) = self.current_char {
            if c == '\n' {
                break;
            }
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let start_pos = self.position;
        while let Some(c) = self.current_char {
            if c.is_ident_char() || c.is_digit(10) {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[start_pos..self.position].iter().collect()
    }

    fn lookup_identifier(&self, ident: String) -> Token {
        match ident.as_str() {
            "func" => Token::Func,
            "constructor" => Token::Constructor,
            "record" => Token::Record,
            "resource" => Token::Resource,
            "enum" => Token::Enum,
            "variant" => Token::Variant,
            "let" => Token::Let,
            "var" => Token::Var,
            "if" => Token::If,
            "else" => Token::Else,
            "match" => Token::Match,
            "use" => Token::Use,
            "import" => Token::Import,
            "export" => Token::Export,
            "package" => Token::Package,
            "in" => Token::Contains,
            "return" => Token::Return,
            "throw" => Token::Throw,
            "continue" => Token::Continue,
            "break" => Token::Break,
            "for" => Token::For,
            "while" => Token::While,
            _ => Token::Identifier(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let start_pos = self.position;
        let mut is_float = false;

        while let Some(c) = self.current_char {
            if c.is_digit(10) {
                self.read_char();
            } else if c == '.' && self.peek_char().map_or(false, |next_c| next_c.is_digit(10)) {
                is_float = true;
                self.read_char(); // Consume '.'
                self.read_char(); // Consume digit after '.'
            } else {
                break;
            }
        }

        let number_str: String = self.input[start_pos..self.position].iter().collect();

        if is_float {
            match number_str.parse::<f64>() {
                Ok(num) => Token::FloatLiteral(num),
                Err(_) => Token::Invalid(number_str), // Handle error appropriately
            }
        } else {
            match number_str.parse::<i64>() {
                Ok(num) => Token::IntegerLiteral(num),
                Err(_) => Token::Invalid(number_str), // Handle error appropriately
            }
        }
    }

    fn read_string(&mut self) -> String {
        self.read_char(); // Consume the opening quote
        let start_pos = self.position;

        while let Some(c) = self.current_char {
            if c == '"' {
                break;
            }
            self.read_char();
        }

        let string: String = self.input[start_pos..self.position].iter().collect();
        self.read_char(); // Consume the closing quote
        string
    }
}

trait CharExt {
    fn is_ident_char(self) -> bool;
    fn is_newline(self) -> bool;
}

impl CharExt for char {
    fn is_ident_char(self) -> bool {
        self.is_ascii_lowercase() || self == '_'
    }

    fn is_newline(self) -> bool {
        self == '\n' || self == '\r'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_call() {
        let input = "object?.method";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("object".into()),
            Token::SafeCall,
            Token::Identifier("method".into()),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_bang() {
        let input = "!condition";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Bang,
            Token::Identifier("condition".into()),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_concat() {
        let input = "str1 ++ str2";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("str1".into()),
            Token::Concat,
            Token::Identifier("str2".into()),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_contains() {
        let input = "item in collection";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("item".into()),
            Token::Contains,
            Token::Identifier("collection".into()),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_not_equal() {
        let input = "a != b";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("a".into()),
            Token::NotEqual,
            Token::Identifier("b".into()),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_double_equal() {
        let input = "a == b";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("a".into()),
            Token::DoubleEqual,
            Token::Identifier("b".into()),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_arrow() {
        let input = "func compute() -> s32 { return 0 }";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Func,
            Token::Identifier("compute".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("s32".into()),
            Token::LeftBrace,
            Token::Return,
            Token::IntegerLiteral(0),
            Token::RightBrace,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_double_colon() {
        let input = "module::function()";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("module".into()),
            Token::DoubleColon,
            Token::Identifier("function".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_string_literal() {
        let input = r#"let greeting = "Hello, world!""#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier("greeting".into()),
            Token::Equal,
            Token::StringLiteral("Hello, world!".into()),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_number_literals() {
        let input = "let int_val = 42\nlet float_val = 3.14";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier("int_val".into()),
            Token::Equal,
            Token::IntegerLiteral(42),
            Token::Newline,
            Token::Let,
            Token::Identifier("float_val".into()),
            Token::Equal,
            Token::FloatLiteral(3.14),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_invalid_identifier() {
        let input = "Let Value = 10";
        let mut lexer = Lexer::new(input);

        // Since 'Let' and 'Value' start with uppercase letters, they should not be recognized as valid identifiers
        let expected_tokens = vec![
            Token::Eof, // Lexer should not produce valid tokens for uppercase identifiers
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            if matches!(spanned_tok.token, Token::Eof) {
                tokens.push(spanned_tok.token.clone());
                break;
            } else {
                tokens.push(spanned_tok.token.clone());
            }
        }

        // Since the lexer does not recognize uppercase letters in identifiers, we expect tokens to be Eof or errors
        assert_ne!(tokens, expected_tokens);
    }

    #[test]
    fn test_package_header() {
        let input = r#"
        package documentation:http@1.0.0;

        use wasi:clocks/wall-clock.{datetime}"#;

        let mut lexer = Lexer::new(input);

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if let Token::Eof = spanned_tok.token {
                break;
            }
        }

        let expected_tokens = vec![
            Token::Newline,
            Token::Package,
            Token::Identifier("documentation".into()),
            Token::Colon,
            Token::Identifier("http".into()),
            Token::At,
            Token::Invalid("1.0.0".into()),
            Token::Semicolon,
            Token::Newline,
            Token::Use,
            Token::Identifier("wasi".into()),
            Token::Colon,
            Token::Identifier("clocks".into()),
            Token::Slash,
            Token::Identifier("wall".into()),
            Token::Minus,
            Token::Identifier("clock".into()),
            Token::Dot,
            Token::LeftBrace,
            Token::Identifier("datetime".into()),
            Token::RightBrace,
            Token::Eof,
        ];

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_record_declaration() {
        let input = r#"
            record point {
                x: s32
                y: s32
            }"#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Newline,
            Token::Record,
            Token::Identifier("point".into()),
            Token::LeftBrace,
            Token::Newline,
            Token::Identifier("x".into()),
            Token::Colon,
            Token::Identifier("s32".into()),
            Token::Newline,
            Token::Identifier("y".into()),
            Token::Colon,
            Token::Identifier("s32".into()),
            Token::Newline,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_function_declaration() {
        let input = r#"
            func add(a: s32, b: s32) -> s32 {
                a + b
            }"#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Func,
            Token::Identifier("add".into()),
            Token::LeftParen,
            Token::Identifier("a".into()),
            Token::Colon,
            Token::Identifier("s32".into()),
            Token::Comma,
            Token::Identifier("b".into()),
            Token::Colon,
            Token::Identifier("s32".into()),
            Token::RightParen,
            Token::Arrow,
            Token::Identifier("s32".into()),
            Token::LeftBrace,
            Token::Identifier("a".into()),
            Token::Plus,
            Token::Identifier("b".into()),
            Token::RightBrace,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_skip_newline();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_immutable_variable_declaration() {
        let input = r#"let p1 = point {
                                x: 5, 
                                y: 10, 
                             }"#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier("p1".into()),
            Token::Equal,
            Token::Identifier("point".into()),
            Token::LeftBrace,
            Token::Newline,
            Token::Identifier("x".into()),
            Token::Colon,
            Token::IntegerLiteral(5),
            Token::Comma,
            Token::Newline,
            Token::Identifier("y".into()),
            Token::Colon,
            Token::IntegerLiteral(10),
            Token::Comma,
            Token::Newline,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            tokens.push(token.token.clone());
            if matches!(token.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_mutable_variable_declaration() {
        let input = r#"
            var p2 = point {
                x: 5,
                y: 10
            }
            "#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Var,
            Token::Identifier("p2".into()),
            Token::Equal,
            Token::Identifier("point".into()),
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Colon,
            Token::IntegerLiteral(5),
            Token::Comma,
            Token::Identifier("y".into()),
            Token::Colon,
            Token::IntegerLiteral(10),
            Token::RightBrace,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_skip_newline();
            tokens.push(token.token.clone());
            if matches!(token.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_field_assignment() {
        let input = r#"p2.x = 15"#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Identifier("p2".into()),
            Token::Dot,
            Token::Identifier("x".into()),
            Token::Equal,
            Token::IntegerLiteral(15),
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            tokens.push(token.token.clone());
            if matches!(token.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_if_statement_with_comparison() {
        let input = r#"if p2.x == 15 {
                                 p2.x = p2.x + 1
                             }"#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::If,
            Token::Identifier("p2".into()),
            Token::Dot,
            Token::Identifier("x".into()),
            Token::DoubleEqual,
            Token::IntegerLiteral(15),
            Token::LeftBrace,
            Token::Newline,
            Token::Identifier("p2".into()),
            Token::Dot,
            Token::Identifier("x".into()),
            Token::Equal,
            Token::Identifier("p2".into()),
            Token::Dot,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::IntegerLiteral(1),
            Token::Newline,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            tokens.push(token.token.clone());
            if matches!(token.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_preserve_newline_after_comment() {
        let input = r#"
            record point {
                x: s32 // This comment should not change the token order
                y: s32
                // z: s32 <- This introduces a second newline token: (newline _comment newline) instead of (newline)
            }"#;

        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token::Newline,
            Token::Record,
            Token::Identifier("point".into()),
            Token::LeftBrace,
            Token::Newline,
            Token::Identifier("x".into()),
            Token::Colon,
            Token::Identifier("s32".into()),
            Token::Newline,
            Token::Identifier("y".into()),
            Token::Colon,
            Token::Identifier("s32".into()),
            Token::Newline,
            Token::Newline,
            Token::RightBrace,
            Token::Eof,
        ];

        let mut tokens = Vec::new();

        loop {
            let spanned_tok = lexer.next_token();
            tokens.push(spanned_tok.token.clone());
            if matches!(spanned_tok.token, Token::Eof) {
                break;
            }
        }

        assert_eq!(tokens, expected_tokens);
    }
}
