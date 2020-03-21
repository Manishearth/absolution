use absolution::TokenStream;
use quote::quote;

// Declare the helper code in the proc macro, wrapping our `add_one_impl`
// function into a `__add_one_impl` custom derive.
absolution_hack::define_absolution_hack!(__add_one_impl => add_one_impl);

fn add_one_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    quote!(1 + (#input)).into()
}
