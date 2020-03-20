use pm::Span;
use proc_macro2 as pm;
use quote::ToTokens;

mod bigint;
mod literal;

/// A stream of [`TokenTree`]s. This is typically what a proc macro will receive
#[derive(Debug, Clone)]
pub struct TokenStream {
    pub tokens: Vec<TokenTree>,
}

/// A single node in the token tree, i.e. a single token. May contain more
/// tokens via [`Group`].
#[derive(Debug, Clone)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

pub use literal::{LitFloat, LitInt, Literal};

/// A group of tokens, typically surrounded by [`Delimiter`]s.
#[derive(Debug, Clone)]
pub struct Group {
    stream: TokenStream,
    delimiter: Delimiter,
    /// The span of the entire group, including delimiters
    span: Span,
    /// The span of the opening delimiter
    span_open: Span,
    /// The span of the closing delimiter
    span_close: Span,
}

pub use pm::Delimiter;

/// An identifier
#[derive(Debug, Clone)]
pub struct Ident {
    span: Span,
    ident: String,
}

/// A punctuation token.
#[derive(Debug, Clone)]
pub struct Punct {
    kind: PunctKind,
    span: Span,
    /// Whether or not it is separated from the proceeding punctuation
    /// token by whitespace
    spacing: Spacing,
}

pub use pm::Spacing;

/// The specific kind of punctuation token
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

pub trait AsNative {
    type Native;
    /// Convert an `absolution` token into its `proc_macro2` counterpart
    fn as_native(&self) -> Self::Native;
}

impl TokenTree {
    /// The span of this token tree
    pub fn span(&self) -> Span {
        match self {
            TokenTree::Group(ref g) => g.span,
            TokenTree::Ident(ref i) => i.span,
            TokenTree::Punct(ref p) => p.span,
            TokenTree::Literal(ref l) => l.span,
        }
    }
}

impl From<pm::TokenStream> for TokenStream {
    fn from(p: pm::TokenStream) -> Self {
        Self {
            tokens: p.into_iter().map(|t| t.into()).collect(),
        }
    }
}

impl AsNative for TokenStream {
    type Native = pm::TokenStream;
    fn as_native(&self) -> pm::TokenStream {
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

impl AsNative for TokenTree {
    type Native = pm::TokenTree;
    fn as_native(&self) -> pm::TokenTree {
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

impl AsNative for Group {
    type Native = pm::Group;
    fn as_native(&self) -> pm::Group {
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

impl AsNative for Ident {
    type Native = pm::Ident;
    fn as_native(&self) -> pm::Ident {
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

impl AsNative for Punct {
    type Native = pm::Punct;
    fn as_native(&self) -> pm::Punct {
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
