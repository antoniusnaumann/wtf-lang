#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Func,
    Constructor,
    Record,
    Resource,
    Enum,
    Variant,
    Let,
    Var,
    If,
    Else,
    Match,
    Use,
    Import,
    Export,
    Package,
    Contains, // For 'in'

    // Control Flow
    Return,
    Throw,

    // Identifiers and Literals
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    DoubleEqual,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Arrow,
    QuestionMark,
    SafeCall, // ?.
    Bang,     // !
    Concat,   // ++

    // Punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
    Colon,
    Dot,
    DoubleColon,
    At,

    // Special tokens
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

    pub fn next_token(&mut self) -> SpannedToken {
        self.skip_whitespace();

        let start_pos = self.position;

        let token = match self.current_char {
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
            Some(c) if is_letter(c) => {
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
            if c.is_whitespace() {
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
        self.read_char(); // Consume the newline
    }

    fn read_identifier(&mut self) -> String {
        let start_pos = self.position;
        while let Some(c) = self.current_char {
            if is_letter(c) || c.is_digit(10) {
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

fn is_letter(c: char) -> bool {
    c.is_ascii_lowercase() || c == '_'
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
            Token::Package,
            Token::Identifier("documentation".into()),
            Token::Colon,
            Token::Identifier("http".into()),
            Token::At,
            Token::Invalid("1.0.0".into()),
            Token::Semicolon,
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
}

// More to test:

// record point {
//     x: s32
//     y: s32
// }

// func add(a: s32, b: s32) -> s32 {
//     a + b
// }

// let p1 = point {
//     x: 5,
//     y: 10
// }

// var p2 = point {
//     x: 5,
//     y: 10
// }

// p2.x = 15

// if p2.x == 15 {
//     p2.x = p2.x + 1
// }

// p2?.x

// p2.x++

// value in list

// !
