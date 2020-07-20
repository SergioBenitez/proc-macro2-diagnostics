use proc_macro2::{Span, TokenStream};

use crate::SpanDiagnosticExt;


/// Trait implemented by types that can be converted into a set of `Span`s.
pub trait MultiSpan {
    /// Converts `self` into a `Vec<Span>`.
    fn into_spans(self) -> Vec<Span>;
}

impl MultiSpan for Span {
    fn into_spans(self) -> Vec<Span> { vec![self] }
}

impl MultiSpan for Vec<Span> {
    fn into_spans(self) -> Vec<Span> { self }
}

impl<'a> MultiSpan for &'a [Span] {
    fn into_spans(self) -> Vec<Span> {
        self.to_vec()
    }
}

/// An enum representing a diagnostic level.
#[non_exhaustive]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Level {
    /// An error.
    Error,
    /// A warning.
    Warning,
    /// A note.
    Note,
    /// A help message.
    Help,
}

impl std::str::FromStr for Level {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains(Level::Error.as_str()) {
            Ok(Level::Error)
        } else if s.contains(Level::Warning.as_str()) {
            Ok(Level::Warning)
        } else if s.contains(Level::Note.as_str()) {
            Ok(Level::Note)
        } else if s.contains(Level::Help.as_str()) {
            Ok(Level::Help)
        } else {
            Err(())
        }
    }
}

impl Level {
    fn as_str(self) -> &'static str {
        match self {
            Level::Error => "error",
            Level::Warning => "warning",
            Level::Note => "note",
            Level::Help => "help",
        }
    }
}

impl std::fmt::Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

struct Colored<'a>(&'static str, Level, &'static str, &'a str);

impl std::fmt::Display for Colored<'_> {
    #[cfg(all(feature = "colors", not(nightly_diagnostics)))]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(windows)]
        static INIT: std::sync::Once = std::sync::Once::new();

        #[cfg(windows)]
        INIT.call_once(|| {
            if cfg!(windows) && !Paint::enable_windows_ascii() {
                Paint::disable();
            }
        });

        use yansi::{Paint, Color};
        let style = match self.1 {
            Level::Error => Color::Red.style().bold(),
            Level::Warning => Color::Yellow.style().bold(),
            Level::Note => Color::Green.style().bold(),
            Level::Help => Color::Cyan.style().bold(),
        };

        write!(f, "{}{}{}{}", self.0, style.paint(self.1), Paint::default(self.2).bold(), self.3)?;
        Color::Default.style().bold().fmt_prefix(f)?;
        Ok(())
    }

    #[cfg(not(all(feature = "colors", not(nightly_diagnostics))))]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}{}", self.0, self.1, self.2, self.3)
    }
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

macro_rules! diagnostic_child_methods {
    ($spanned:ident, $regular:ident, $level:expr) => (
        /// Adds a new child diagnostic message to `self` with the level
        /// identified by this method's name with the given `spans` and
        /// `message`.
        pub fn $spanned<S, T>(self, spans: S, message: T) -> Diagnostic
            where S: MultiSpan, T: Into<String>
        {
            self.spanned_child(spans, $level, message)
        }

        /// Adds a new child diagnostic message to `self` with the level
        /// identified by this method's name with the given `message`.
        pub fn $regular<T: Into<String>>(self, message: T) -> Diagnostic {
            self.child($level, message)
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

    /// Adds a new child diagnostic message to `self` with the `level` and the
    /// given `spans` and `message`.
    pub fn spanned_child<S, T>(mut self, spans: S, level: Level, message: T) -> Diagnostic
        where S: MultiSpan, T: Into<String>
    {
        self.children.push(Diagnostic::spanned(spans, level, message));
        self
    }

    /// Adds a new child diagnostic message to `self` with `level` and the given
    /// `message`.
    pub fn child<T: Into<String>>(mut self, level: Level, message: T) -> Diagnostic {
        self.children.push(Diagnostic::new(level, message));
        self
    }

    diagnostic_child_methods!(span_error, error, Level::Error);
    diagnostic_child_methods!(span_warning, warning, Level::Warning);
    diagnostic_child_methods!(span_note, note, Level::Note);
    diagnostic_child_methods!(span_help, help, Level::Help);

    /// Return the children diagnostics of `self`.
    pub fn children(&self) -> impl Iterator<Item=&Diagnostic> {
        self.children.iter()
    }

    /// Return the `level` of `self`.
    pub fn level(&self) -> Level {
        self.level
    }

    fn stable_emit_as_tokens(self) -> TokenStream {
        let error: syn::parse::Error = self.into();
        error.to_compile_error()
    }

    /// Emit the diagnostic as tokens.
    #[cfg(not(nightly_diagnostics))]
    pub fn emit_as_tokens(self) -> TokenStream {
        self.stable_emit_as_tokens()
    }

    /// Emit the diagnostic as tokens.
    #[cfg(nightly_diagnostics)]
    pub fn emit_as_tokens(self) -> TokenStream {
        if !crate::nightly_works() {
            return self.stable_emit_as_tokens();
        }

        let nightly_diag: proc_macro::Diagnostic = self.into();
        nightly_diag.emit();
        TokenStream::new()
    }
}

const NEW_PREFIX: &str = "[";
const NEW_SUFFIX: &str = "] ";

const JOIN_PREFIX: &str = "--- ";
const JOIN_SUFFIX: &str = ": ";

impl From<Diagnostic> for syn::parse::Error {
    fn from(diag: Diagnostic) -> syn::parse::Error {
        fn diag_to_msg(diag: &Diagnostic) -> String {
            let (spans, level, msg) = (&diag.spans, diag.level, &diag.message);
            if spans.is_empty() {
                Colored(JOIN_PREFIX, level, JOIN_SUFFIX, msg).to_string()
            } else {
                if level == Level::Error {
                    return msg.into();
                }

                Colored(NEW_PREFIX, level, NEW_SUFFIX, msg).to_string()
            }
        }

        fn diag_to_span(diag: &Diagnostic) -> Span {
            diag.spans.get(0).cloned().unwrap_or_else(|| Span::call_site())
        }

        let mut msg = diag_to_msg(&diag);
        let mut span = diag_to_span(&diag);
        let mut error: Option<syn::Error> = None;
        for child in diag.children {
            if child.spans.is_empty() {
                // Join to the current error we're building up.
                msg.push_str(&format!("\n  {}", diag_to_msg(&child)));
            } else {
                // This creates a new error with all of the diagnostic messages
                // that have been joined thus far in `msg`.
                let new_error = syn::parse::Error::new(span, &msg);
                if let Some(ref mut error) = error {
                    error.combine(new_error);
                } else {
                    error = Some(new_error);
                }

                // Start a new error to be built from `child`.
                span = diag_to_span(&child);
                msg = diag_to_msg(&child);
            }
        }

        if let Some(mut error) = error {
            error.combine(syn::parse::Error::new(span, &msg));
            error
        } else {
            syn::parse::Error::new(span, &msg)
        }
    }
}

impl From<syn::parse::Error> for Diagnostic {
    fn from(error: syn::parse::Error) -> Diagnostic {
        fn parse<'a>(msg: &'a str, prefix: &str, suffix: &str) -> Option<(Level, &'a str)> {
            if msg.starts_with(prefix) {
                let end = msg.find(suffix)?;
                let level: Level = msg[prefix.len()..end].parse().ok()?;
                let msg = &msg[end + suffix.len()..];
                Some((level, msg))
            } else {
                None
            }
        }

        let mut diag: Option<Diagnostic> = None;
        for e in &error {
            for line in e.to_string().lines() {
                let msg = line.trim_start();
                if let Some((level, msg)) = parse(msg, JOIN_PREFIX, JOIN_SUFFIX) {
                    diag = diag.map(|d| d.child(level, msg));
                } else if let Some((level, msg)) = parse(msg, NEW_PREFIX, NEW_SUFFIX) {
                    diag = diag.map(|d| d.spanned_child(e.span(), level, msg))
                        .or_else(|| Some(Diagnostic::spanned(e.span(), level, msg)));
                } else {
                    diag = diag.map(|d| d.span_error(e.span(), line))
                        .or_else(|| Some(e.span().error(line)));
                }
            }
        }

        diag.unwrap_or_else(|| error.span().error(error.to_string()))
    }
}

#[cfg(nightly_diagnostics)]
impl Into<proc_macro::Diagnostic> for Diagnostic {
    fn into(self) -> proc_macro::Diagnostic {
        fn spans_to_proc_macro_spans(spans: Vec<Span>) -> Vec<proc_macro::Span> {
            spans.into_iter()
                .map(|s| s.unstable())
                .collect::<Vec<proc_macro::Span>>()
        }

        let spans = spans_to_proc_macro_spans(self.spans);

        let level = match self.level {
            Level::Error => proc_macro::Level::Error,
            Level::Warning => proc_macro::Level::Warning,
            Level::Note => proc_macro::Level::Note,
            Level::Help => proc_macro::Level::Help,
        };

        let mut diag = proc_macro::Diagnostic::spanned(spans, level, self.message);
        for child in self.children {
            // FIXME: proc_macro::Diagnostic needs a `push` method.
            let spans = spans_to_proc_macro_spans(child.spans);
            diag = match child.level {
                Level::Error => diag.span_error(spans, child.message),
                Level::Warning => diag.span_warning(spans, child.message),
                Level::Note => diag.span_note(spans, child.message),
                Level::Help => diag.span_help(spans, child.message),
            };
        }

        diag
    }
}
