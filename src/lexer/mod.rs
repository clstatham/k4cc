use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::{multispace0, multispace1, one_of},
    combinator::*,
    multi::{many0, many1},
    sequence::{delimited, tuple},
    IResult, InputIter, InputLength, InputTake,
};

use nom_locate::LocatedSpan;
use tokens::*;

pub mod tokens;

pub type Span<'a> = LocatedSpan<&'a [u8]>;

pub const NONDIGIT: &[u8] = b"_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
pub const DIGIT: &[u8] = b"0123456789";

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Illegal(Span<'a>),
    EOI(Span<'a>),
    Ident(Span<'a>),
    Constant(Span<'a>, Constant),
    String(Span<'a>),
    Keyword(Span<'a>, Keyword),
    Punctuator(Span<'a>, Punctuator),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Constant {
    Integer(i64),
    FloatingPoint(f64),
    Character(char),
}

pub fn lex_ident(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    map(
        recognize(tuple((
            one_of(NONDIGIT),
            many0(alt((one_of(NONDIGIT), one_of(DIGIT)))),
        ))),
        Token::Ident,
    )(inp)
}

pub fn lex_integer(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    map(recognize(many1(one_of(DIGIT))), |s: Span<'_>| {
        Token::Constant(
            inp,
            Constant::Integer(std::str::from_utf8(&s).unwrap().parse::<i64>().unwrap()),
        )
    })(inp)
}

pub fn lex_illegal(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    map(take(1usize), Token::Illegal)(inp)
}

pub fn lex_token(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    alt((
        lex_punctuator,
        lex_keyword,
        lex_ident,
        lex_integer,
        lex_illegal,
    ))(inp)
}

pub fn lex_tokens(inp: Span<'_>) -> IResult<Span<'_>, Vec<Token<'_>>> {
    many0(delimited(multispace0, lex_token, multispace0))(inp)
        .map(|(s, res)| (s, [&res[..], &[Token::EOI(s)][..]].concat()))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Tokens<'a> {
    pub tok: &'a [Token<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(tok: &'a [Token<'a>]) -> Self {
        Self {
            tok,
            start: 0,
            end: tok.len(),
        }
    }
}

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

impl<'a> InputLength for Token<'a> {
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
    type Item = &'a Token<'a>;
    type Iter = std::iter::Enumerate<std::slice::Iter<'a, Token<'a>>>;
    type IterElem = std::slice::Iter<'a, Token<'a>>;

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
        let inp = "int main(void) {
            return 1 + 2;
        }"
        .as_bytes();
        let (_, toks) = lex_tokens(Span::new(inp)).unwrap();
        dbg!(toks);
    }
}
