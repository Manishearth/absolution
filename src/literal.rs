// Imported from syn
// Original source: https://github.com/dtolnay/syn/blob/04b7618556d3699e47edf517933376595ad8e2ac/src/lit.rs#L875

use proc_macro2 as pm;

use proc_macro2::Span;

use crate::bigint::BigInt;
use std::char;
use std::ops::{Index, RangeFrom};

/// A literal token
#[derive(Debug, Clone)]
pub struct Literal {
    pub kind: LitKind,
    pub span: Span,
}

// XXXManishearth technically all literals except booleans
// can have arbitrary suffixes, however we only support them for
// ints and floats right now

#[derive(Debug, Clone)]
pub enum LitKind {
    /// A string (`"foo"`)
    Str(Box<str>),
    /// A byte string (`b"foo"`)
    ByteStr(Vec<u8>),
    /// A byte literal (`b'a'`)
    Byte(u8),
    /// A character literal (`'a'`)
    Char(char),
    /// An integer (`5`)
    Int(LitInt),
    /// A boolean (`true`)
    Bool(bool),
    /// A float (`3.4`)
    Float(LitFloat),
}

/// An integer literal
#[derive(Debug, Clone)]
pub struct LitInt {
    /// The digits of the integer. Please use
    /// `.parse()` to obtain the appropriate integer
    digits: Box<str>,
    /// The type suffix, if any
    suffix: IntSuffix,
}

/// The type suffix of an integer literal
#[derive(Debug, Clone)]
pub enum IntSuffix {
    /// No suffix (e.g. `1`)
    Unsuffixed,
    /// A `u8` suffix (e.g. `10_u8`)
    U8,
    /// A `u16` suffix (e.g. `10_u16`)
    U16,
    /// A `u32` suffix (e.g. `10_u32`)
    U32,
    /// A `u64` suffix (e.g. `10_u64`)
    U64,
    /// A `u128` suffix (e.g. `10_u128`)
    U128,
    /// A `uSize` suffix (e.g. `10_uSize`)
    USize,
    /// A `i8` suffix (e.g. `10_i8`)
    I8,
    /// A `i16` suffix (e.g. `10_i16`)
    I16,
    /// A `i32` suffix (e.g. `10_i32`)
    I32,
    /// A `i64` suffix (e.g. `10_i64`)
    I64,
    /// A `i128` suffix (e.g. `10_i128`)
    I128,
    /// A `iSize` suffix (e.g. `10_iSize`)
    ISize,
    /// Some other suffix (e.g. `10_foobar`)
    UnknownSuffix(Box<str>),
}

// A float literal
#[derive(Debug, Clone)]
pub struct LitFloat {
    /// The digits of the float. Please use
    /// `.parse()` to obtain the appropriate float
    digits: Box<str>,
    /// The type suffix, if any
    suffix: Box<str>,
}

/// The type suffix of a float literal
#[derive(Debug, Clone)]
pub enum FloatSuffix {
    /// No suffix (e.g. `1.0`)
    Unsuffixed,
    /// An `f32` suffix (e.g. `10_f32`)
    F32,
    /// An `f64` suffix (e.g. `10_f64`)
    F64,
    /// Some other suffix (e.g. `1.0_foobar`)
    UnknownSuffix(Box<str>),
}

impl From<Box<str>> for IntSuffix {
    fn from(s: Box<str>) -> Self {
        use IntSuffix::*;
        match &*s {
            "" => Unsuffixed,
            "u8" => U8,
            "u16" => U16,
            "u32" => U32,
            "u64" => U64,
            "u128" => U128,
            "usize" => USize,
            "i8" => I8,
            "i16" => I16,
            "i32" => I32,
            "i64" => I64,
            "i128" => I128,
            "isize" => ISize,
            _ => UnknownSuffix(s),
        }
    }
}

impl From<Box<str>> for FloatSuffix {
    fn from(s: Box<str>) -> Self {
        use FloatSuffix::*;
        match &*s {
            "" => Unsuffixed,
            "f32" => F32,
            "f64" => F64,
            _ => UnknownSuffix(s),
        }
    }
}

impl From<pm::Literal> for Literal {
    fn from(token: pm::Literal) -> Self {
        let span = token.span();
        let kind = LitKind::from_pm(token);
        Literal { span, kind }
    }
}

impl super::AsNative for Literal {
    type Native = pm::Literal;
    fn as_native(&self) -> pm::Literal {
        unimplemented!()
    }
}

impl LitKind {
    fn from_pm(token: pm::Literal) -> Self {
        let repr = token.to_string();
        match byte(&repr, 0) {
            b'"' | b'r' => {
                let (string, suffix) = parse_lit_str(&repr);
                if !suffix.is_empty() {
                    panic!("`absolution` does not support parsing strings with suffixes")
                }
                return LitKind::Str(string);
            }
            b'b' => match byte(&repr, 1) {
                b'"' | b'r' => {
                    return LitKind::ByteStr(parse_lit_byte_str(&repr));
                }
                b'\'' => {
                    return LitKind::Byte(parse_lit_byte(&repr));
                }
                _ => {}
            },
            b'\'' => {
                return LitKind::Char(parse_lit_char(&repr));
            }
            b'0'..=b'9' | b'-' => {
                if !(repr.ends_with("f32") || repr.ends_with("f64")) {
                    if let Some((digits, suffix)) = parse_lit_int(&repr) {
                        return LitKind::Int(LitInt {
                            digits,
                            suffix: suffix.into(),
                        });
                    }
                }
                if let Some((digits, suffix)) = parse_lit_float(&repr) {
                    return LitKind::Float(LitFloat {
                        digits,
                        suffix: suffix.into(),
                    });
                }
            }
            b't' | b'f' => {
                if repr == "true" || repr == "false" {
                    return LitKind::Bool(repr == "true");
                }
            }
            _ => {}
        }
        panic!("Unrecognized literal: `{}`", repr)
    }
}

/// Get the byte at offset idx, or a default of `b'\0'` if we're looking
/// past the end of the input buffer.
pub fn byte<S: AsRef<[u8]> + ?Sized>(s: &S, idx: usize) -> u8 {
    let s = s.as_ref();
    if idx < s.len() {
        s[idx]
    } else {
        0
    }
}

fn next_chr(s: &str) -> char {
    s.chars().next().unwrap_or('\0')
}

// Returns (content, suffix).
pub fn parse_lit_str(s: &str) -> (Box<str>, Box<str>) {
    match byte(s, 0) {
        b'"' => parse_lit_str_cooked(s),
        b'r' => parse_lit_str_raw(s),
        _ => unreachable!(),
    }
}

// Clippy false positive
// https://github.com/rust-lang-nursery/rust-clippy/issues/2329
#[allow(clippy::needless_continue)]
fn parse_lit_str_cooked(mut s: &str) -> (Box<str>, Box<str>) {
    assert_eq!(byte(s, 0), b'"');
    s = &s[1..];

    let mut content = String::new();
    'outer: loop {
        let ch = match byte(s, 0) {
            b'"' => break,
            b'\\' => {
                let b = byte(s, 1);
                s = &s[2..];
                match b {
                    b'x' => {
                        let (byte, rest) = backslash_x(s);
                        s = rest;
                        assert!(byte <= 0x80, "Invalid \\x byte in string literal");
                        char::from_u32(u32::from(byte)).unwrap()
                    }
                    b'u' => {
                        let (chr, rest) = backslash_u(s);
                        s = rest;
                        chr
                    }
                    b'n' => '\n',
                    b'r' => '\r',
                    b't' => '\t',
                    b'\\' => '\\',
                    b'0' => '\0',
                    b'\'' => '\'',
                    b'"' => '"',
                    b'\r' | b'\n' => loop {
                        let ch = next_chr(s);
                        if ch.is_whitespace() {
                            s = &s[ch.len_utf8()..];
                        } else {
                            continue 'outer;
                        }
                    },
                    b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
                }
            }
            b'\r' => {
                assert_eq!(byte(s, 1), b'\n', "Bare CR not allowed in string");
                s = &s[2..];
                '\n'
            }
            _ => {
                let ch = next_chr(s);
                s = &s[ch.len_utf8()..];
                ch
            }
        };
        content.push(ch);
    }

    assert!(s.starts_with('"'));
    let content = content.into_boxed_str();
    let suffix = s[1..].to_owned().into_boxed_str();
    (content, suffix)
}

fn parse_lit_str_raw(mut s: &str) -> (Box<str>, Box<str>) {
    assert_eq!(byte(s, 0), b'r');
    s = &s[1..];

    let mut pounds = 0;
    while byte(s, pounds) == b'#' {
        pounds += 1;
    }
    assert_eq!(byte(s, pounds), b'"');
    assert_eq!(byte(s, s.len() - pounds - 1), b'"');
    for end in s[s.len() - pounds..].bytes() {
        assert_eq!(end, b'#');
    }

    let content = s[pounds + 1..s.len() - pounds - 1]
        .to_owned()
        .into_boxed_str();
    let suffix = Box::<str>::default(); // todo
    (content, suffix)
}

pub fn parse_lit_byte_str(s: &str) -> Vec<u8> {
    assert_eq!(byte(s, 0), b'b');
    match byte(s, 1) {
        b'"' => parse_lit_byte_str_cooked(s),
        b'r' => parse_lit_byte_str_raw(s),
        _ => unreachable!(),
    }
}

// Clippy false positive
// https://github.com/rust-lang-nursery/rust-clippy/issues/2329
#[allow(clippy::needless_continue)]
fn parse_lit_byte_str_cooked(mut s: &str) -> Vec<u8> {
    assert_eq!(byte(s, 0), b'b');
    assert_eq!(byte(s, 1), b'"');
    s = &s[2..];

    // We're going to want to have slices which don't respect codepoint boundaries.
    let mut s = s.as_bytes();

    let mut out = Vec::new();
    'outer: loop {
        let byte = match byte(s, 0) {
            b'"' => break,
            b'\\' => {
                let b = byte(s, 1);
                s = &s[2..];
                match b {
                    b'x' => {
                        let (b, rest) = backslash_x(s);
                        s = rest;
                        b
                    }
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'\\' => b'\\',
                    b'0' => b'\0',
                    b'\'' => b'\'',
                    b'"' => b'"',
                    b'\r' | b'\n' => loop {
                        let byte = byte(s, 0);
                        let ch = char::from_u32(u32::from(byte)).unwrap();
                        if ch.is_whitespace() {
                            s = &s[1..];
                        } else {
                            continue 'outer;
                        }
                    },
                    b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
                }
            }
            b'\r' => {
                assert_eq!(byte(s, 1), b'\n', "Bare CR not allowed in string");
                s = &s[2..];
                b'\n'
            }
            b => {
                s = &s[1..];
                b
            }
        };
        out.push(byte);
    }

    assert_eq!(s, b"\"");
    out
}

fn parse_lit_byte_str_raw(s: &str) -> Vec<u8> {
    assert_eq!(byte(s, 0), b'b');
    String::from(parse_lit_str_raw(&s[1..]).0).into_bytes()
}

pub fn parse_lit_byte(s: &str) -> u8 {
    assert_eq!(byte(s, 0), b'b');
    assert_eq!(byte(s, 1), b'\'');

    // We're going to want to have slices which don't respect codepoint boundaries.
    let mut s = s[2..].as_bytes();

    let b = match byte(s, 0) {
        b'\\' => {
            let b = byte(s, 1);
            s = &s[2..];
            match b {
                b'x' => {
                    let (b, rest) = backslash_x(s);
                    s = rest;
                    b
                }
                b'n' => b'\n',
                b'r' => b'\r',
                b't' => b'\t',
                b'\\' => b'\\',
                b'0' => b'\0',
                b'\'' => b'\'',
                b'"' => b'"',
                b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
            }
        }
        b => {
            s = &s[1..];
            b
        }
    };

    assert_eq!(byte(s, 0), b'\'');
    b
}

pub fn parse_lit_char(mut s: &str) -> char {
    assert_eq!(byte(s, 0), b'\'');
    s = &s[1..];

    let ch = match byte(s, 0) {
        b'\\' => {
            let b = byte(s, 1);
            s = &s[2..];
            match b {
                b'x' => {
                    let (byte, rest) = backslash_x(s);
                    s = rest;
                    assert!(byte <= 0x80, "Invalid \\x byte in string literal");
                    char::from_u32(u32::from(byte)).unwrap()
                }
                b'u' => {
                    let (chr, rest) = backslash_u(s);
                    s = rest;
                    chr
                }
                b'n' => '\n',
                b'r' => '\r',
                b't' => '\t',
                b'\\' => '\\',
                b'0' => '\0',
                b'\'' => '\'',
                b'"' => '"',
                b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
            }
        }
        _ => {
            let ch = next_chr(s);
            s = &s[ch.len_utf8()..];
            ch
        }
    };
    assert_eq!(s, "\'", "Expected end of char literal");
    ch
}

fn backslash_x<S>(s: &S) -> (u8, &S)
where
    S: Index<RangeFrom<usize>, Output = S> + AsRef<[u8]> + ?Sized,
{
    let mut ch = 0;
    let b0 = byte(s, 0);
    let b1 = byte(s, 1);
    ch += 0x10
        * match b0 {
            b'0'..=b'9' => b0 - b'0',
            b'a'..=b'f' => 10 + (b0 - b'a'),
            b'A'..=b'F' => 10 + (b0 - b'A'),
            _ => panic!("unexpected non-hex character after \\x"),
        };
    ch += match b1 {
        b'0'..=b'9' => b1 - b'0',
        b'a'..=b'f' => 10 + (b1 - b'a'),
        b'A'..=b'F' => 10 + (b1 - b'A'),
        _ => panic!("unexpected non-hex character after \\x"),
    };
    (ch, &s[2..])
}

fn backslash_u(mut s: &str) -> (char, &str) {
    if byte(s, 0) != b'{' {
        panic!("expected {{ after \\u");
    }
    s = &s[1..];

    let mut ch = 0;
    for _ in 0..6 {
        let b = byte(s, 0);
        match b {
            b'0'..=b'9' => {
                ch *= 0x10;
                ch += u32::from(b - b'0');
                s = &s[1..];
            }
            b'a'..=b'f' => {
                ch *= 0x10;
                ch += u32::from(10 + b - b'a');
                s = &s[1..];
            }
            b'A'..=b'F' => {
                ch *= 0x10;
                ch += u32::from(10 + b - b'A');
                s = &s[1..];
            }
            b'}' => break,
            _ => panic!("unexpected non-hex character after \\u"),
        }
    }
    assert!(byte(s, 0) == b'}');
    s = &s[1..];

    if let Some(ch) = char::from_u32(ch) {
        (ch, s)
    } else {
        panic!("character code {:x} is not a valid unicode character", ch);
    }
}

// Returns base 10 digits and suffix.
pub fn parse_lit_int(mut s: &str) -> Option<(Box<str>, Box<str>)> {
    let negative = byte(s, 0) == b'-';
    if negative {
        s = &s[1..];
    }

    let base = match (byte(s, 0), byte(s, 1)) {
        (b'0', b'x') => {
            s = &s[2..];
            16
        }
        (b'0', b'o') => {
            s = &s[2..];
            8
        }
        (b'0', b'b') => {
            s = &s[2..];
            2
        }
        (b'0'..=b'9', _) => 10,
        _ => return None,
    };

    let mut value = BigInt::new();
    loop {
        let b = byte(s, 0);
        let digit = match b {
            b'0'..=b'9' => b - b'0',
            b'a'..=b'f' if base > 10 => b - b'a' + 10,
            b'A'..=b'F' if base > 10 => b - b'A' + 10,
            b'_' => {
                s = &s[1..];
                continue;
            }
            // NOTE: Looking at a floating point literal, we don't want to
            // consider these integers.
            b'.' if base == 10 => return None,
            b'e' | b'E' if base == 10 => return None,
            _ => break,
        };

        if digit >= base {
            return None;
        }

        value *= base;
        value += digit;
        s = &s[1..];
    }

    let suffix = s;
    if suffix.is_empty() || id_ok(&suffix) {
        let mut repr = value.to_string();
        if negative {
            repr.insert(0, '-');
        }
        Some((repr.into_boxed_str(), suffix.to_owned().into_boxed_str()))
    } else {
        None
    }
}

// Returns base 10 digits and suffix.
pub fn parse_lit_float(input: &str) -> Option<(Box<str>, Box<str>)> {
    // Rust's floating point literals are very similar to the ones parsed by
    // the standard library, except that rust's literals can contain
    // ignorable underscores. Let's remove those underscores.

    let mut bytes = input.to_owned().into_bytes();

    let start = (*bytes.get(0)? == b'-') as usize;
    match bytes.get(start)? {
        b'0'..=b'9' => {}
        _ => return None,
    }

    let mut read = start;
    let mut write = start;
    let mut has_dot = false;
    let mut has_e = false;
    let mut has_sign = false;
    let mut has_exponent = false;
    while read < bytes.len() {
        match bytes[read] {
            b'_' => {
                // Don't increase write
                read += 1;
                continue;
            }
            b'0'..=b'9' => {
                if has_e {
                    has_exponent = true;
                }
                bytes[write] = bytes[read];
            }
            b'.' => {
                if has_e || has_dot {
                    return None;
                }
                has_dot = true;
                bytes[write] = b'.';
            }
            b'e' | b'E' => {
                if has_e {
                    return None;
                }
                has_e = true;
                bytes[write] = b'e';
            }
            b'-' | b'+' => {
                if has_sign || has_exponent || !has_e {
                    return None;
                }
                has_sign = true;
                if bytes[read] == b'-' {
                    bytes[write] = bytes[read];
                } else {
                    // Omit '+'
                    read += 1;
                    continue;
                }
            }
            _ => break,
        }
        read += 1;
        write += 1;
    }

    if has_e && !has_exponent {
        return None;
    }

    let mut digits = String::from_utf8(bytes).unwrap();
    let suffix = digits.split_off(read);
    digits.truncate(write);
    if suffix.is_empty() || id_ok(&suffix) {
        Some((digits.into_boxed_str(), suffix.into_boxed_str()))
    } else {
        None
    }
}

pub fn id_ok(symbol: &str) -> bool {
    let mut chars = symbol.chars();
    let first = chars.next().unwrap();
    if !(first.is_alphabetic() || first == '_') {
        return false;
    }
    for ch in chars {
        if !(ch.is_alphanumeric() || ch == '_') {
            return false;
        }
    }
    true
}
