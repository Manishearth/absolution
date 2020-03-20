use absolution::TokenStream;
use proc_macro2;
use quote::{quote, ToTokens};

fn test_roundtrip(pm: proc_macro2::TokenStream) {
    let ab: TokenStream = pm.clone().into();
    assert_eq!(pm.to_string(), ab.to_token_stream().to_string());
    assert_eq!(pm.to_string(), quote!(#ab).to_string());
}

#[test]
fn roundtrip_basic() {
    test_roundtrip(quote!(1));
    test_roundtrip(quote!("foo"));
    test_roundtrip(quote!("foo" + 5));
    test_roundtrip(quote!(5u8));
    test_roundtrip(quote!(5f32));
    test_roundtrip(quote!("hello"));
    test_roundtrip(quote!(1 2 3 -1));
    test_roundtrip(quote!(1 + (1 - five)));
    test_roundtrip(quote!(1 += 3(foo("hi"))));
}
