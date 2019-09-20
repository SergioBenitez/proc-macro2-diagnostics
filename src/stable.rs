use proc_macro2::{Span, TokenStream};

use crate::{SpanDiagExt, Spanned};
use crate::{WARN_PREFIX, NOTE_PREFIX, HELP_PREFIX};
use crate::{JOINED_WARN_PREFIX, JOINED_NOTE_PREFIX, JOINED_HELP_PREFIX, JOINED_ERROR_PREFIX};

/// An enum representing a diagnostic level.
#[derive(Copy, Clone, Debug)]
pub enum Level {
    /// An error.
    Error,
    /// A warning.
    Warning,
    /// A note.
    Note,
    /// A help message.
    Help,
    #[doc(hidden)]
    __NonExhaustive
}

/// A structure representing a diagnostic message and associated children
/// messages.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    level: Level,
    message: String,
    spans: Vec<Span>,
    children: Vec<Diagnostic>
}

/// Trait implemented by types that can be converted into a set of `Span`s.
pub trait MultiSpan {
    /// Converts `self` into a `Vec<Span>`.
    fn into_spans(self) -> Vec<Span>;
}

impl MultiSpan for Span {
    fn into_spans(self) -> Vec<Span> { vec![self] }
}

macro_rules! diagnostic_child_methods {
    ($spanned:ident, $regular:ident, $level:expr) => (
        /// Adds a new child diagnostic message to `self` with the level
        /// identified by this method's name with the given `spans` and
        /// `message`.
        pub fn $spanned<S, T>(mut self, spans: S, message: T) -> Diagnostic
            where S: MultiSpan, T: Into<String>
        {
            self.children.push(Diagnostic::spanned(spans, $level, message));
            self
        }

        /// Adds a new child diagnostic message to `self` with the level
        /// identified by this method's name with the given `message`.
        pub fn $regular<T: Into<String>>(mut self, message: T) -> Diagnostic {
            self.children.push(Diagnostic::new($level, message));
            self
        }
    )
}

impl Diagnostic {
    /// Creates a new diagnostic with the given `level` and `message`.
    pub fn new<T: Into<String>>(level: Level, message: T) -> Diagnostic {
        Diagnostic {
            level: level,
            message: message.into(),
            spans: vec![],
            children: vec![]
        }
    }

    /// Creates a new diagnostic with the given `level` and `message` pointing
    /// to the given set of `spans`.
    pub fn spanned<S, T>(spans: S, level: Level, message: T) -> Diagnostic
        where S: MultiSpan, T: Into<String>
    {
        Diagnostic {
            level: level,
            message: message.into(),
            spans: spans.into_spans(),
            children: vec![]
        }
    }

    diagnostic_child_methods!(span_error, error, Level::Error);
    diagnostic_child_methods!(span_warning, warning, Level::Warning);
    diagnostic_child_methods!(span_note, note, Level::Note);
    diagnostic_child_methods!(span_help, help, Level::Help);

    /// Return the `level` of `self`.
    pub fn level(&self) -> Level {
        self.level
    }

    /// Emit the diagnostic.
    pub fn emit_as_tokens(self) -> TokenStream {
        let syn_error: syn::parse::Error = self.into();
        syn_error.to_compile_error()
    }
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

    fn join(&self, _other: Span) -> Option<Span> {
        Some(self.clone())
    }
}

impl From<::syn::parse::Error> for Diagnostic {
    fn from(errors: syn::parse::Error) -> Diagnostic {
        let mut diag = errors.span().error(errors.to_string());
        for e in errors.into_iter().skip(1) {
            diag = diag.span_error(e.span(), e.to_string());
        }

        diag
    }
}

impl Into<syn::parse::Error> for Diagnostic {
    fn into(self) -> syn::parse::Error {
        fn diag_to_msg(diag: &Diagnostic) -> String {
            let spans = &diag.spans;
            let msg_prefix = match (spans.is_empty(), diag.level) {
                (true, Level::Warning) => JOINED_WARN_PREFIX,
                (false, Level::Warning) => WARN_PREFIX,
                (true, Level::Note) => JOINED_NOTE_PREFIX,
                (false, Level::Note) => NOTE_PREFIX,
                (true, Level::Help) => JOINED_HELP_PREFIX,
                (false, Level::Help) => HELP_PREFIX,
                (true, Level::Error) => JOINED_ERROR_PREFIX,
                _ => ""
            };

            format!("{}{}", msg_prefix, diag.message)
        }

        fn diag_to_span(diag: &Diagnostic) -> Span {
            let spans = &diag.spans;
            if spans.is_empty() {
                Span::call_site()
            } else {
                spans[0]
            }
        }

        let span = diag_to_span(&self);
        let mut error = syn::parse::Error::new(span, diag_to_msg(&self));
        for child in self.children {
            let child_error = if child.spans.is_empty() {
                syn::parse::Error::new(span, diag_to_msg(&child))
            } else {
                syn::parse::Error::new(diag_to_span(&child), diag_to_msg(&child))
            };

            error.combine(child_error);
        }

        error
    }
}

impl<T: quote::ToTokens> Spanned for T {
    #[inline(always)]
    fn span(&self) -> Span {
        syn::spanned::Spanned::span(self)
    }
}
