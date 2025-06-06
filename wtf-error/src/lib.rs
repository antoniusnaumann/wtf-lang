use wtf_tokens::{Span, Token};

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    // pub source: PathBuf,
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    /// Identifier is not in scope
    UnknownIdentifier,

    /// Expected one of 'expected', got 'found'
    UnexpectedToken { found: Token, expected: Vec<Token> },

    /// Expected one of 'expected', got 'found'
    ///
    /// 'found' can be an identifier that the user confused for a keyword
    UnexpectedKeyword { found: Token, expected: Vec<Token> },
}

impl Error {
    pub fn with_source(&self, source: &[char]) -> String {
        // TODO: Add line and column number
        let position_hint = format!("at ({}, {})", self.span.start, self.span.end);
        let span_snippet = source[self.span.start..self.span.end]
            .iter()
            .collect::<String>()
            .trim()
            .to_owned();
        // TODO: Give line for context
        match &self.kind {
            ErrorKind::UnknownIdentifier => {
                format!("Unknown Identifier '{span_snippet}' {position_hint}")
            }
            ErrorKind::UnexpectedToken { found, expected }
            | ErrorKind::UnexpectedKeyword { found, expected } => {
                let tokens = if expected.len() == 1 {
                    expected[0].category_name()
                } else {
                    &format!(
                        "one of: [{}]",
                        &expected[0..expected.len()]
                            .iter()
                            .map(|t| t.category_name())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };
                format!("Expected {tokens} but found '{found}' {position_hint}")
            }
        }
    }
}
