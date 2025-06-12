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

pub trait WithSpan<T> {
    fn spanned(self, span: Span) -> Spanned<T>;
}

impl<T: Sized> WithSpan<T> for T {
    fn spanned(self, span: Span) -> Spanned<T> {
        Spanned::new(self, span)
    }
}
