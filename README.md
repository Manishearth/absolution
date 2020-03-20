## absolution

[![Build Status](https://github.com/Manishearth/absolution/workflows/Tests/badge.svg)](https://github.com/manishearth/absolution/actions)
[![Current Version](https://meritbadge.herokuapp.com/absolution)](https://crates.io/crates/absolution)
[![License: MIT/Apache-2.0](https://img.shields.io/crates/l/absolution.svg)](#license)

"Freedom from [syn](https://github.com/dtolnay/syn/)"

This crate provides an introspectible token tree representation for writing Rust proc macros. It's still somewhat unstable: It's usable, however I haven't quite decided what I want the API to look like and may publish breaking changes. I welcome feedback of all kinds!

The [`proc-macro2`](https://docs.rs/proc-macro2/) crate provides a token tree representation, however for backwards compatibility reasons, this representation isn't very introspectible and you have to further parse it to do almost anything with it. For example, the [`Literal`](https://docs.rs/proc-macro2/1.0.9/proc_macro2/struct.Literal.html) type does not actually expose any further information about the contained literal, only that it is a literal.


Typically people pull in the awesome [syn](https://github.com/dtolnay/syn/) crate if they wish to introspect the code further than this; `syn` parses Rust code into a full, introspectible AST that is easy to work with. This is great when writing custom derives, where your proc macro is being applied to some Rust item.

However, bang-style procedural macros, like `format!()`, typically don't need to introspect the Rust AST, they just need to look at the tokens. For example, a very simple `format!()` implementation just needs to be able to read the initial format string, and then get the comma delimited token trees for the arguments. Pulling in an entire Rust parser into your dependency tree for this is somewhat overkill.

`absolution` provides an introspectible token tree representation, structured similarly to that of `proc-macro2`, for use in designing bang-style procedural macros.

Currently the [`proc-macro-hack`](https://docs.rs/proc-macro-hack/) crate still relies on `syn` to work, so this crate is not yet useful for any attribute-style proc macro.

To use, simply use `Into` to convert from `proc_macro` or `proc_macro2` token streams to `absolution` ones, and `quote!` to convert back.

```rust
extern crate proc_macro;
use absolution::{Ident, LitKind, Literal, TokenStream, TokenTree};
use quote::quote;

#[proc_macro]
pub fn make_func(tt: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let stream: TokenStream = tt.into();
    let first_token = &stream.tokens[0];
    let s = if let TokenTree::Literal(Literal {
        kind: LitKind::Str(s),
        ..
    }) = &first_token
    {
        s
    } else {
        panic!("Must start with a string!")
    };

    let ident = Ident {
        ident: s.to_string(),
        span: first_token.span(),
    };

    quote!(
        fn #ident() -> &'static str {
            #first_token
        }
    )
    .into()
}
```

See [`examples/string-enum`](github.com/manishearth/absolution/tree/master/examples/string-enum) for more examples

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>
