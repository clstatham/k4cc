use std::{
    fmt::Debug,
    ops::{Range, RangeFrom, RangeFull, RangeTo},
};

use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::{multispace0, one_of},
    combinator::*,
    error::{ErrorKind, FromExternalError, ParseError, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, tuple},
    InputIter, InputLength, InputTake,
};

use crate::LexResult;

use gen::*;
use nom_locate::LocatedSpan;

pub mod gen;

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub struct Span<'a> {
//     pub source: &'a str,
//     pub span: LocatedSpan<&'a str>,
// }

// impl<'a> std::ops::Deref for Span<'a> {
//     type Target = LocatedSpan<&'a str>;
//     fn deref(&self) -> &Self::Target {
//         &self.span
//     }
// }

// impl<'a> Span<'a> {
//     pub fn new(source: &'a str, span: LocatedSpan<&'a str>) -> Self {
//         Self { source, span }
//     }

//     pub fn span(&self) -> LocatedSpan<&'a str> {
//         self.span
//     }
// }

// impl<'a> InputLength for Span<'a> {
//     fn input_len(&self) -> usize {
//         self.span.input_len()
//     }
// }

// impl<'a> InputTake for Span<'a> {
//     fn take(&self, count: usize) -> Self {
//         Self::new(self.source, self.span.take(count))
//     }

//     fn take_split(&self, count: usize) -> (Self, Self) {
//         let (a, b) = self.span.take_split(count);
//         (Self::new(self.source, a), Self::new(self.source, b))
//     }
// }

// impl<'a> InputIter for Span<'a> {
//     type Item = u8;
//     type Iter = std::iter::Enumerate<u8>;
// }

pub type Span<'a> = LocatedSpan<&'a str, &'a str>;

pub const NONDIGIT: &str = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
pub const DIGIT: &str = "0123456789";

#[derive(PartialEq, Clone, Copy)]
pub struct WithSpan<'a, T: 'a>(pub Span<'a>, pub T);

impl<'a, T: 'a> WithSpan<'a, T> {
    pub fn span(&self) -> Span<'a> {
        self.0
    }

    pub fn item(&self) -> &T {
        &self.1
    }
}

impl<'a, T: 'a> Debug for WithSpan<'a, T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({}:{}) {:#?}",
            self.span().location_offset(),
            self.span().location_line(),
            self.span().get_utf8_column(),
            self.item(),
        )
    }
}

#[derive(PartialEq, Clone, Copy)]
pub struct Ident<'a>(pub &'a str);

impl<'a> Debug for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Illegal,
    EOI,
    Ident(Ident<'a>),
    Constant(Constant),
    String,
    Keyword(Keyword),
    Punctuator(Punctuator),
}

pub type SpanToken<'a> = WithSpan<'a, Token<'a>>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Constant {
    Integer(i64),
    // FloatingPoint(f64),
    // Character(char),
}

pub fn lex_ident(inp: Span<'_>) -> LexResult<Span<'_>, SpanToken<'_>> {
    map(
        recognize(tuple((
            one_of(NONDIGIT),
            many0(alt((one_of(NONDIGIT), one_of(DIGIT)))),
        ))),
        |span| WithSpan(span, Token::Ident(Ident(span.fragment()))),
    )(inp)
}

pub fn lex_integer(inp: Span<'_>) -> LexResult<Span<'_>, SpanToken<'_>> {
    map(recognize(many1(one_of(DIGIT))), |span: Span<'_>| {
        WithSpan(
            span,
            Token::Constant(Constant::Integer(span.parse::<i64>().unwrap())),
        )
    })(inp)
}

pub fn lex_illegal(inp: Span<'_>) -> LexResult<Span<'_>, SpanToken<'_>> {
    map(take(1usize), |span| WithSpan(span, Token::Illegal))(inp)
}

pub fn lex_token(inp: Span<'_>) -> LexResult<Span<'_>, SpanToken<'_>> {
    alt((
        lex_punctuator,
        lex_keyword,
        lex_ident,
        lex_integer,
        lex_illegal,
    ))(inp)
}

pub fn lex_tokens(inp: Span<'_>) -> LexResult<Span<'_>, Vec<SpanToken<'_>>> {
    many0(delimited(multispace0, lex_token, multispace0))(inp)
        .map(|(s, res)| (s, [&res[..], &[WithSpan(s, Token::EOI)][..]].concat()))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Tokens<'a> {
    pub tok: &'a [SpanToken<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(tok: &'a [SpanToken<'a>]) -> Self {
        Self {
            tok,
            start: 0,
            end: tok.len(),
        }
    }

    pub fn first_span(self) -> Span<'a> {
        self.tok[0].span()
    }

    pub fn first_into<T: 'a>(self, t: T) -> WithSpan<'a, T> {
        WithSpan(self.first_span(), t)
    }
}

// #[macro_export]
// macro_rules! expect_next {
//     ($inp:expr, $expected:pat, $pretty_name:literal, $success:expr) => {
//         let (rest, one) = nom::bytes::complete::take(1usize)($inp)?;
//         if one.tok.is_empty() {
//             use nom::error::FromExternalError;
//             return Err(nom::Err::Failure(Error::from_external_error(
//                 $inp,
//                 ErrorKind::Tag,
//                 format!("Expected {:?}", $pretty_name),
//             )));
//         } else if let $expected = one.tok[0].item() {
//             return $success(one.tok[0].span(), rest);
//         } else {
//             use nom::error::FromExternalError;
//             return Err(nom::Err::Failure(Error::from_external_error(
//                 $inp,
//                 ErrorKind::Tag,
//                 one.tok[0].fmt_error(&format!("Expected `{:?}`", $pretty_name)),
//             )));
//         }
//     };
// }

impl<'a> InputLength for Tokens<'a> {
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            tok: &self.tok[..count],
            start: 0,
            end: count,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (pre, suf) = self.tok.split_at(count);
        let first = Self {
            tok: pre,
            start: 0,
            end: pre.len(),
        };
        let second = Self {
            tok: suf,
            start: 0,
            end: suf.len(),
        };
        (second, first)
    }
}

impl<'a> InputLength for SpanToken<'a> {
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> nom::Slice<Range<usize>> for Tokens<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        Self {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> nom::Slice<RangeTo<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> nom::Slice<RangeFrom<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> nom::Slice<RangeFull> for Tokens<'a> {
    fn slice(&self, _: RangeFull) -> Self {
        Self {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a SpanToken<'a>;
    type Iter = std::iter::Enumerate<std::slice::Iter<'a, SpanToken<'a>>>;
    type IterElem = std::slice::Iter<'a, SpanToken<'a>>;

    fn iter_indices(&self) -> Self::Iter {
        self.tok.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tok.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_1() {
        let inp = "int main() {
            return 1 + 2;
        }";
        let inp = Span::new_extra(inp, inp);
        let (_, toks) = lex_tokens(inp).unwrap();
        dbg!(toks);
    }
}
