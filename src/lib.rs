use pm::Span;
use proc_macro2 as pm;
use quote::ToTokens;

mod bigint;
mod literal;

#[derive(Debug, Clone)]
pub struct TokenStream {
    pub tokens: Vec<TokenTree>,
}

#[derive(Debug, Clone)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

pub use literal::{LitFloat, LitInt, Literal};

#[derive(Debug, Clone)]
pub struct Group {
    stream: TokenStream,
    delimiter: pm::Delimiter,
    span: Span,
    span_open: Span,
    span_close: Span,
}

#[derive(Debug, Clone)]
pub struct Ident {
    span: Span,
    ident: String,
}

#[derive(Debug, Clone)]
pub struct Punct {
    kind: PunctKind,
    span: Span,
    spacing: pm::Spacing,
}

#[derive(Debug, Clone)]
pub enum PunctKind {
    /// `;`
    Semicolon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `@`
    At,
    /// `~`
    Tilde,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `$`
    Dollar,
    /// `=`
    Eq,
    /// `!`
    Not,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `-`
    Minus,
    /// `&`
    And,
    /// `|`
    Or,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,
}

impl From<pm::TokenStream> for TokenStream {
    fn from(p: pm::TokenStream) -> Self {
        Self {
            tokens: p.into_iter().map(|t| t.into()).collect(),
        }
    }
}

impl TokenStream {
    pub fn as_native(&self) -> pm::TokenStream {
        self.tokens.iter().map(|t| t.as_native()).collect()
    }
}

impl From<pm::TokenTree> for TokenTree {
    fn from(p: pm::TokenTree) -> Self {
        match p {
            pm::TokenTree::Group(g) => TokenTree::Group(g.into()),
            pm::TokenTree::Ident(i) => TokenTree::Ident(i.into()),
            pm::TokenTree::Punct(p) => TokenTree::Punct(p.into()),
            pm::TokenTree::Literal(l) => TokenTree::Literal(l.into()),
        }
    }
}

impl TokenTree {
    pub fn as_native(&self) -> pm::TokenTree {
        match self {
            TokenTree::Group(ref g) => pm::TokenTree::Group(g.as_native()),
            TokenTree::Ident(ref i) => pm::TokenTree::Ident(i.as_native()),
            TokenTree::Punct(ref p) => pm::TokenTree::Punct(p.as_native()),
            TokenTree::Literal(ref l) => pm::TokenTree::Literal(l.as_native()),
        }
    }
}

impl From<pm::Group> for Group {
    fn from(p: pm::Group) -> Self {
        let span = p.span();
        let span_open = p.span_open();
        let span_close = p.span_close();
        let delimiter = p.delimiter();
        let stream = p.stream().into();
        Self {
            span,
            span_open,
            span_close,
            delimiter,
            stream,
        }
    }
}

impl Group {
    pub fn as_native(&self) -> pm::Group {
        let mut g = pm::Group::new(self.delimiter, self.stream.as_native());
        g.set_span(self.span);
        g
    }
}

impl From<pm::Ident> for Ident {
    fn from(p: pm::Ident) -> Self {
        // XXXManishearth strip out and note down the r# of raw idents
        let span = p.span();
        let ident = p.to_string();
        Self { span, ident }
    }
}

impl Ident {
    pub fn as_native(&self) -> pm::Ident {
        pm::Ident::new(&self.ident, self.span)
    }
}

impl From<pm::Punct> for Punct {
    fn from(p: pm::Punct) -> Self {
        use PunctKind::*;
        let span = p.span();
        let spacing = p.spacing();

        let kind = match p.as_char() {
            ';' => Semicolon,
            ',' => Comma,
            '.' => Dot,
            '@' => At,
            '~' => Tilde,
            '?' => Question,
            ':' => Colon,
            '$' => Dollar,
            '=' => Eq,
            '!' => Not,
            '<' => Lt,
            '>' => Gt,
            '-' => Minus,
            '&' => And,
            '|' => Or,
            '+' => Plus,
            '*' => Star,
            '/' => Slash,
            '^' => Caret,
            '%' => Percent,
            x => panic!("Found unknown punctuation token: `{}`", x),
        };
        Self {
            kind,
            spacing,
            span,
        }
    }
}

impl Punct {
    pub fn as_native(&self) -> pm::Punct {
        pm::Punct::new(self.kind.as_char(), self.spacing)
    }
}

impl PunctKind {
    pub fn as_char(&self) -> char {
        use PunctKind::*;
        match *self {
            Semicolon => ';',
            Comma => ',',
            Dot => '.',
            At => '@',
            Tilde => '~',
            Question => '?',
            Colon => ':',
            Dollar => '$',
            Eq => '=',
            Not => '!',
            Lt => '<',
            Gt => '>',
            Minus => '-',
            And => '&',
            Or => '|',
            Plus => '+',
            Star => '*',
            Slash => '/',
            Caret => '^',
            Percent => '%',
        }
    }
}

macro_rules! totokens_impl {
    ($($ty:ident),+) => {
        $(
            impl ToTokens for $ty {
                fn to_tokens(&self, tokens: &mut pm::TokenStream) {
                    self.as_native().to_tokens(tokens)
                }
            }
        )+
    };
}

totokens_impl!(TokenStream, TokenTree, Group, Ident, Punct, Literal);
