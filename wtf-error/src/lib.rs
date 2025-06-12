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

    /// Function signature not found for the given arguments
    UnknownFunction { name: String },

    /// Field does not exist on the given type
    UnknownField { field: String, type_name: String },

    /// Trying to assign to an immutable variable
    ImmutableAssignment { name: String },

    /// Type mismatch in operations or assignments
    TypeMismatch { expected: String, found: String },

    /// Using unsupported operations on types
    UnsupportedOperation { operation: String, type_name: String },
}

impl Error {
    pub fn unknown_identifier(span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::UnknownIdentifier,
        }
    }

    pub fn unknown_function(name: String, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::UnknownFunction { name },
        }
    }

    pub fn unknown_field(field: String, type_name: String, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::UnknownField { field, type_name },
        }
    }

    pub fn immutable_assignment(name: String, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::ImmutableAssignment { name },
        }
    }

    pub fn type_mismatch(expected: String, found: String, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::TypeMismatch { expected, found },
        }
    }

    pub fn unsupported_operation(operation: String, type_name: String, span: Span) -> Self {
        Self {
            span,
            kind: ErrorKind::UnsupportedOperation { operation, type_name },
        }
    }

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
            ErrorKind::UnknownFunction { name } => {
                format!("Unknown function '{name}' {position_hint}")
            }
            ErrorKind::UnknownField { field, type_name } => {
                format!("Field '{field}' does not exist on type '{type_name}' {position_hint}")
            }
            ErrorKind::ImmutableAssignment { name } => {
                format!("Cannot assign to immutable variable '{name}' {position_hint}")
            }
            ErrorKind::TypeMismatch { expected, found } => {
                format!("Type mismatch: expected '{expected}', found '{found}' {position_hint}")
            }
            ErrorKind::UnsupportedOperation { operation, type_name } => {
                format!("Unsupported operation '{operation}' on type '{type_name}' {position_hint}")
            }
        }
    }
}
