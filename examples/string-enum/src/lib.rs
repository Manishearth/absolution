extern crate proc_macro;
use absolution::{Ident, LitKind, Punct, PunctKind, TokenStream, TokenTree};
use quote::quote;

#[proc_macro]
pub fn make_enum(tt: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let stream: TokenStream = tt.into();
    let mut variants = vec![];
    let mut arms = vec![];
    let enum_name = if let TokenTree::Ident(ref i) = stream.tokens[0] {
        i.clone()
    } else {
        panic!("syntax: make_enum!(EnumName: \"variant1\", \"variant2\", ...)");
    };

    if let TokenTree::Punct(Punct {
        kind: PunctKind::Colon,
        ..
    }) = stream.tokens[1]
    {
    } else {
        panic!("syntax: make_enum!(EnumName: \"variant1\", \"variant2\", ...)");
    }

    for token in &stream.tokens[2..] {
        match token {
            TokenTree::Punct(p) => {
                if p.kind == PunctKind::Comma {
                    continue;
                } else {
                    panic!("Unexpected token: {:?}", token)
                }
            }
            TokenTree::Literal(l) => {
                if let LitKind::Str(ref s) = l.kind {
                    let (left, right) = s.split_at(1);
                    let name = left.to_ascii_uppercase() + &right.to_ascii_lowercase();
                    let ident = Ident {
                        span: token.span(),
                        ident: name.to_string(),
                    };
                    let ident_docs = Ident {
                        span: token.span(),
                        ident: s.to_string(),
                    };
                    variants.push(quote!(
                        #[doc(#ident_docs)]
                        #ident,
                    ));

                    arms.push(quote!(
                        #enum_name::#ident => #token,
                    ))
                }
            }
            _ => panic!("Unexpected token: {:?}", token),
        }
    }

    quote!(
        pub enum #enum_name {
            #(#variants)*
        }

        impl #enum_name {
            pub fn as_str(&self) -> &'static str {
                match *self {
                    #(#arms)*
                }
            }
        }

    )
    .into()
}
