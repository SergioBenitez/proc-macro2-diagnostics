extern crate proc_macro;

use proc_macro2::Span;
use proc_macro2_diagnostics::{SpanDiagnosticExt, Diagnostic, Level};

struct Invocation {
    items: syn::punctuated::Punctuated<Item, syn::Token![,]>,
}

enum Kind {
    Error,
    Warning,
    Note,
    Help,
    _Error,
    _Warning,
    _Note,
    _Help,
}

struct Item {
    kind: Kind,
    span: Span,
    message: String
}

impl syn::parse::Parse for Kind {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let name = input.parse::<syn::Ident>()?;
        match &*name.to_string() {
            "error" => Ok(Kind::Error),
            "warning" => Ok(Kind::Warning),
            "note" => Ok(Kind::Note),
            "help" => Ok(Kind::Help),
            "_error" => Ok(Kind::_Error),
            "_warning" => Ok(Kind::_Warning),
            "_note" => Ok(Kind::_Note),
            "_help" => Ok(Kind::_Help),
            _ => {
                Err(name.span()
                    .error("unknown diagnostic kind")
                    .help("expected one of: 'error', 'warning', 'note', 'help'")
                    .note("or their _ versions")
                    .into())
            }
        }
    }
}

impl syn::parse::Parse for Item {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let kind = input.parse::<Kind>()?;
        input.parse::<syn::Token![:]>()?;

        let mut message = String::new();
        let mut span = input.cursor().span();
        while !input.peek(syn::Token![,]) && !input.is_empty() {
            let ident = input.parse::<syn::Ident>()?;
            span = match span.join(ident.span()) {
                Some(span) => span,
                None => span
            };

            message.push_str(&ident.to_string());
            message.push(' ');
        }

        Ok(Item { kind, span, message })
    }
}

impl syn::parse::Parse for Invocation {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        Ok(Invocation { items: input.parse_terminated(Item::parse, syn::Token![,])?, })
    }
}

fn diagnostic(tokens: proc_macro::TokenStream) -> Diagnostic {
    let input = match syn::parse::<Invocation>(tokens) {
        Ok(input) => input,
        Err(e) => return e.into()
    };

    let mut diagnostic: Option<Diagnostic> = None;
    for item in input.items {
        let message = item.message.trim();
        if let Some(diag) = diagnostic.take() {
            diagnostic = Some(match item.kind {
                Kind::Error => diag.span_error(item.span, message),
                Kind::Warning => diag.span_warning(item.span, message),
                Kind::Help => diag.span_help(item.span, message),
                Kind::Note => diag.span_note(item.span, message),
                Kind::_Error => diag.error(message),
                Kind::_Warning => diag.warning(message),
                Kind::_Help => diag.help(message),
                Kind::_Note => diag.note(message),
            });
        } else {
            let level = match item.kind {
                Kind::Error | Kind::_Error => Level::Error,
                Kind::Warning | Kind::_Warning => Level::Warning,
                Kind::Help | Kind::_Help => Level::Help,
                Kind::Note | Kind::_Note => Level::Note,
            };

            diagnostic = Some(Diagnostic::spanned(item.span, level, message));
        }
    }

    diagnostic.unwrap_or_else(|| Span::call_site().error("expected diagnostic"))
}

#[proc_macro]
pub fn diagnostic_item(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    diagnostic(tokens).emit_as_item_tokens().into()
}

#[proc_macro]
pub fn diagnostic_expr(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    diagnostic(tokens).emit_as_expr_tokens().into()
}
