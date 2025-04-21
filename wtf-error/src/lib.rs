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
    UnexpectedToken { expected: Vec<Token> },

    /// Expected one of 'expected', got 'found'
    ///
    /// 'found' can be an identifier that the user confused for a keyword
    UnexpectedKeyword { expected: Vec<Token> },
}

impl Error {
    pub fn with_source(&self, source: &[char]) -> String {
        // TODO: Add line and column number
        let position_hint = format!("At ({}, {})", self.span.start, self.span.end);
        let span_snippet: String = source[self.span.start..self.span.end].iter().collect();
        // TODO: Give line for context
        match &self.kind {
            ErrorKind::UnknownIdentifier => {
                format!("{position_hint}\n\tUnknown Identifier '{span_snippet}'")
            }
            ErrorKind::UnexpectedToken { expected } => {
                let tokens = if expected.len() == 1 {
                    expected[0].category_name()
                } else {
                    &expected[0..expected.len()]
                        .iter()
                        .map(|t| t.category_name())
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                format!("{position_hint}\n\tExpected {tokens} but found '{span_snippet}'")
            }
            ErrorKind::UnexpectedKeyword { expected } => todo!(),
        }
    }
}
