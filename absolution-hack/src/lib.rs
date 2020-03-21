//! Helper macros for declaring and defining a `proc-macro-hack`-like macro
//! without depending on `syn`.
//!
//! Unlike `proc-macro-hack`, this crate makes no effort to provide advanced
//! features, like nested macro support, or customized hygiene. If you need
//! these advanced features, use the `proc-macro-hack` crate.

/// Used in the wrapper crate.
#[macro_export]
macro_rules! declare_absolution_hack {
    (
        $(#[$m:meta])*
        pub use $krate:ident :: $real_name:ident as $name:ident;
    ) => {
        $crate::declare_hack_internal!([$] [$(#[$m])*] $krate $real_name $name);
    };
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! declare_hack_internal {
    ([$dlr:tt] [$($m:tt)*] $krate:ident $real_name:ident $name:ident) => {
        #[doc(hidden)]
        pub use $krate :: $real_name;

        $($m)*
        #[macro_export]
        macro_rules! $name {
            // ($($proc_macro:tt)*) =>
            ($dlr($dlr proc_macro:tt)*) => {{
                // Call the custom derive from within an expression block.
                #[derive($dlr crate :: $real_name)]
                enum AbsolutionHack {
                    Value = (stringify! { $dlr($dlr proc_macro)* }, 0).1,
                }
                // Invoke the generated `macro_rules!` macro
                absolution_hack_call!()
            }};
        }
    };
}

/// Used in the wrappee crate.
#[macro_export]
macro_rules! define_absolution_hack {
    ($name:ident => $func:ident) => {
        // Declare our method which consumes the `TokenStream`.
        //
        // The logic is declared entirely within the target crate, despite it
        // being possible to declare a helper method which would simplify
        // things, in order to avoid having this crate directly depend on
        // `proc_macro`, as it is also depended on by the declaration crate.
        #[proc_macro_derive($name)]
        pub fn $name(input: ::proc_macro::TokenStream) -> ::proc_macro::TokenStream {
            use ::proc_macro::{
                Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree,
            };
            use ::std::iter::FromIterator;

            let mut iter = input.into_iter();
            iter.next().unwrap(); // `enum`
            iter.next().unwrap(); // `AbsolutionHack`

            // Enter { ... }
            let mut iter = match iter.next().unwrap() {
                TokenTree::Group(group) => group.stream().into_iter(),
                _ => unimplemented!(),
            };
            iter.next().unwrap(); // `Value`
            iter.next().unwrap(); // `=`

            // Enter ( ... )
            let mut iter = match iter.next().unwrap() {
                TokenTree::Group(group) => group.stream().into_iter(),
                _ => unimplemented!(),
            };
            iter.next().unwrap(); // `stringify`
            iter.next().unwrap(); // `!`

            // Get stream from { ... }
            let input = match iter.next().unwrap() {
                TokenTree::Group(group) => group.stream(),
                _ => unimplemented!(),
            };

            // Invoke the real macro implementation.
            let output: TokenStream = $func(input.clone());

            // Wrap in a dummy macro invocation:
            // macro_rules! absolution_hack_call {
            //     () => { #output }
            // }
            TokenStream::from_iter(vec![
                TokenTree::Ident(Ident::new("macro_rules", Span::call_site())),
                TokenTree::Punct(Punct::new('!', Spacing::Alone)),
                TokenTree::Ident(Ident::new(
                    "absolution_hack_call",
                    ::proc_macro::Span::call_site(),
                )),
                TokenTree::Group(Group::new(
                    Delimiter::Brace,
                    TokenStream::from_iter(vec![
                        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                        TokenTree::Punct(Punct::new('=', Spacing::Joint)),
                        TokenTree::Punct(Punct::new('>', Spacing::Alone)),
                        TokenTree::Group(Group::new(Delimiter::Brace, output)),
                    ]),
                )),
            ])
        }
    };
}
