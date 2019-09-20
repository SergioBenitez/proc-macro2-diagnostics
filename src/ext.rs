use proc_macro2::Span;

use crate::diagnostic::{Level, Diagnostic};

pub trait SpanDiagExt {
    fn error<T: Into<String>>(self, message: T) -> Diagnostic;
    fn warning<T: Into<String>>(self, message: T) -> Diagnostic;
    fn note<T: Into<String>>(self, message: T) -> Diagnostic;
    fn help<T: Into<String>>(self, message: T) -> Diagnostic;
}

macro_rules! diagnostic_method {
    ($name:ident, $level:expr) => (
        /// Creates a new `Diagnostic` with the given `message` at the span
        /// `self`.
        fn $name<T: Into<String>>(self, message: T) -> Diagnostic {
            Diagnostic::spanned(self, $level, message)
        }
    )
}

impl SpanDiagExt for Span {
    diagnostic_method!(error, Level::Error);
    diagnostic_method!(warning, Level::Warning);
    diagnostic_method!(note, Level::Note);
    diagnostic_method!(help, Level::Help);
}
