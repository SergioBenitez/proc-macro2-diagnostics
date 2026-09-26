# proc-macro2-diagnostics &thinsp; [![crates.io]][crate] [![docs.rs]][docs]

[crates.io]: https://img.shields.io/crates/v/proc-macro2-diagnostics.svg
[crate]: https://crates.io/crates/proc-macro2-diagnostics
[docs.rs]: https://docs.rs/proc-macro2-diagnostics/badge.svg
[docs]: https://docs.rs/proc-macro2-diagnostics

Diagnostics for stable and nightly proc-macros!

## Usage

1. Depend on the library in your proc-macro.

```toml
[dependencies]
proc-macro2-diagnostics = "0.11"
```

2. Import `SpanDiagnosticExt` and use its methods on a `proc_macro2::Span` to
   create `Diagnostic`s:

```rust
use syn::spanned::Spanned;
use proc_macro2::TokenStream;
use proc_macro2_diagnostics::{SpanDiagnosticExt, Diagnostic};

fn my_macro(input: TokenStream) -> Result<TokenStream, Diagnostic> {
    Err(input.span().error("there's a problem here..."))
}
```

3. If there's an error, emit the diagnostic as tokens:

```rust
extern crate proc_macro;

pub fn real_macro(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match my_macro(tokens.into()) {
        Ok(tokens) => tokens.into(),
        Err(diag) => diag.emit_as_expr_tokens().into()
    }
}
```

This does the right thing on nightly _or_ stable.

### Caveats and Nightly

By default, diagnostics use stable APIs irrespective of the running compiler.
Due to limitations with stable APIs, any top-level, non-error diagnostic is
emitted as an error, which will immediately abort compilation. On a stable
compiler, this cannot be avoided: you may want to `cfg`-gate emitting non-error
diagnostics to nightly. On nightly, you can opt-in to using nightly APIs by
enabling the `nightly` feature:

```toml
proc-macro2-diagnostics = { version = "0.11", features = ["nightly"] }
```

Aside from supporting non-error top-level diagnostics, nightly APIs allow for
more precise error attribution, at the cost of using unstable compiler APIs,
which may change or break future compilation. Enabling the feature on stable has
no effect.

### Colors

The compiler controls diagnostic coloring on both stable and nightly. Use
Cargo's `--color=always` or `--color=never` to override automatic coloring. When
stable APIs are in use, `note`, `help`, and `warning` labels are never colored.

## License

Licensed under either of the following, at your option:

  * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
  * MIT License ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
