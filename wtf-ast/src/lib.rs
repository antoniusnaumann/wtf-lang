mod ast;
pub use ast::*;

mod display;

use wtf_tokens::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}

/// A convenience type alias for spanned expressions
pub type SpannedExpression = Spanned<Expression>;
