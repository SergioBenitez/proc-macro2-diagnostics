#![cfg_attr(nightly_diagnostics, feature(proc_macro_diagnostic, proc_macro_span))]

extern crate proc_macro;

use proc_macro2::Span;

#[cfg(not(nightly_diagnostics))] mod stable;
#[cfg(not(nightly_diagnostics))] pub use stable::{Diagnostic, Level};

#[cfg(nightly_diagnostics)] mod nightly;
#[cfg(nightly_diagnostics)] pub use nightly::{Diagnostic, Level};

const WARN_PREFIX: &str = "[warning] ";
const JOINED_WARN_PREFIX: &str = "[<- warning] ";

const NOTE_PREFIX: &str = "[note] ";
const JOINED_NOTE_PREFIX: &str = "[<- note] ";

const HELP_PREFIX: &str = "[help] ";
const JOINED_HELP_PREFIX: &str = "[<- help] ";

const JOINED_ERROR_PREFIX: &str = "[<- error] ";

pub trait SpanDiagExt {
    fn error<T: Into<String>>(self, message: T) -> Diagnostic;
    fn warning<T: Into<String>>(self, message: T) -> Diagnostic;
    fn note<T: Into<String>>(self, message: T) -> Diagnostic;
    fn help<T: Into<String>>(self, message: T) -> Diagnostic;
    fn join(&self, other: Span) -> Option<Span>;
}

mod private {
    pub trait Sealed {}
    impl<T: quote::ToTokens> Sealed for T {}
}

pub trait Spanned: private::Sealed {
    fn span(&self) -> Span;
}
