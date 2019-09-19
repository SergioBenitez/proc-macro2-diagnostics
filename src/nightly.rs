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

#[doc(hidden)]
impl Into<proc_macro::Level> for Level {
    fn into(self) -> proc_macro::Level {
        match self {
            Level::Error => proc_macro::Level::Error,
            Level::Warning => proc_macro::Level::Warning,
            Level::Note => proc_macro::Level::Note,
            Level::Help => proc_macro::Level::Help,
            Level::__NonExhaustive => unreachable!("never constructed")
        }
    }
}

/// A structure representing a diagnostic message and associated children
/// messages.
#[derive(Debug, Clone)]
pub struct Diagnostic(proc_macro::Diagnostic);

macro_rules! span_ext_method {
    ($name:ident) => (
        fn $name<T: Into<String>>(self, message: T) -> Diagnostic {
            Diagnostic(self.unstable().$name(message))
        }
    )
}

impl SpanDiagExt for Span {
    span_ext_method!(error);
    span_ext_method!(warning);
    span_ext_method!(note);
    span_ext_method!(help);

    fn join(&self, other: Span) -> Option<Span> {
        self.unstable().join(other.unstable()).map(|span| span.into())
    }
}

macro_rules! diagnostic_child_methods {
    ($spanned:ident, $regular:ident) => (
        pub fn $spanned<S, T>(self, spans: S, message: T) -> Diagnostic
            where S: MultiSpan, T: Into<String>
        {
            let inner = self.0;
            let spans = spans.into_spans();
            Diagnostic(inner.$spanned(spans, message))
        }

        /// Adds a new child diagnostic message to `self` with the level
        /// identified by this method's name with the given `message`.
        pub fn $regular<T: Into<String>>(self, message: T) -> Diagnostic {
            let inner = self.0;
            Diagnostic(inner.$regular(message))
        }
    )
}

/// Trait implemented by types that can be converted into a set of `Span`s.
pub trait MultiSpan {
    /// Converts `self` into a `Vec<Span>`.
    fn into_spans(self) -> Vec<proc_macro::Span>;
}

impl MultiSpan for Span {
    fn into_spans(self) -> Vec<proc_macro::Span> { vec![self.unstable()] }
}

impl Diagnostic {
    /// Creates a new diagnostic with the given `level` and `message`.
    pub fn new<T: Into<String>>(level: Level, message: T) -> Diagnostic {
        Diagnostic(proc_macro::Diagnostic::new(level.into(), message))
    }

    /// Creates a new diagnostic with the given `level` and `message` pointing
    /// to the given set of `spans`.
    pub fn spanned<S, T>(spans: S, level: Level, message: T) -> Diagnostic
        where S: MultiSpan, T: Into<String>
    {
        let level = level.into();
        let spans = spans.into_spans();
        Diagnostic(proc_macro::Diagnostic::spanned(spans, level, message))
    }

    diagnostic_child_methods!(span_error, error);
    diagnostic_child_methods!(span_warning, warning);
    diagnostic_child_methods!(span_note, note);
    diagnostic_child_methods!(span_help, help);

    /// Emit the diagnostic.
    pub fn emit_as_tokens(self) -> TokenStream {
        self.0.emit();
        proc_macro2::TokenStream::new()
    }
}

impl From<syn::parse::Error> for Diagnostic {
    fn from(errors: syn::parse::Error) -> Diagnostic {
        let mut diag = errors.span().unstable().error(errors.to_string());
        for e in errors.into_iter().skip(1) {
            let message = e.to_string();
            if message.starts_with(WARN_PREFIX) {
                let message = &message[WARN_PREFIX.len()..];
                diag = diag.span_warning(e.span().unstable(), message.to_string());
            } else if message.starts_with(JOINED_WARN_PREFIX) {
                let message = &message[JOINED_WARN_PREFIX.len()..];
                diag = diag.warning(message.to_string());
            } else if message.starts_with(NOTE_PREFIX) {
                let message = &message[NOTE_PREFIX.len()..];
                diag = diag.span_note(e.span().unstable(), message.to_string());
            } else if message.starts_with(JOINED_NOTE_PREFIX) {
                let message = &message[JOINED_NOTE_PREFIX.len()..];
                diag = diag.note(message.to_string());
            } else if message.starts_with(HELP_PREFIX) {
                let message = &message[HELP_PREFIX.len()..];
                diag = diag.span_help(e.span().unstable(), message.to_string());
            } else if message.starts_with(JOINED_HELP_PREFIX) {
                let message = &message[JOINED_HELP_PREFIX.len()..];
                diag = diag.help(message.to_string());
            } else if message.starts_with(JOINED_ERROR_PREFIX) {
                let message = &message[JOINED_ERROR_PREFIX.len()..];
                diag = diag.error(message.to_string());
            } else {
                diag = diag.span_error(e.span().unstable(), e.to_string());
            }
        }

        Diagnostic(diag)
    }
}

impl Into<::syn::parse::Error> for Diagnostic {
    fn into(self) -> syn::parse::Error {
        let spans = self.0.spans();
        let span = if spans.is_empty() {
            proc_macro::Span::call_site()
        } else {
            spans[0]
        };

        let msg_prefix = match (spans.is_empty(), self.0.level()) {
            (true, proc_macro::Level::Warning) => JOINED_WARN_PREFIX,
            (false, proc_macro::Level::Warning) => WARN_PREFIX,
            (true, proc_macro::Level::Note) => JOINED_NOTE_PREFIX,
            (false, proc_macro::Level::Note) => NOTE_PREFIX,
            (true, proc_macro::Level::Help) => JOINED_HELP_PREFIX,
            (false, proc_macro::Level::Help) => HELP_PREFIX,
            (true, proc_macro::Level::Error) => JOINED_ERROR_PREFIX,
            _ => ""
        };

        let message = format!("{}{}", msg_prefix, self.0.message());
        let mut error = syn::parse::Error::new(span.into(), message);
        for child in self.0.children() {
            error.combine(Diagnostic(child.clone()).into());
        }

        error
    }
}

impl<T: quote::ToTokens> Spanned for T {
    fn span(&self) -> Span {
        let mut tokens = TokenStream::new();
        self.to_tokens(&mut tokens);
        let mut iter = tokens.into_iter();
        let mut span = match iter.next() {
            Some(tt) => tt.span().unstable(),
            None => {
                return Span::call_site();
            }
        };

        for tt in iter {
            if let Some(joined) = span.join(tt.span().unstable()) {
                span = joined;
            }
        }

        span.into()
    }
}
