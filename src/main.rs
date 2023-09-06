use std::fmt::Debug;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use lexer::{Span, Token, Tokens, WithSpan};
use nom::error::ErrorKind;

#[macro_use]
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Token,
    Constant,
    Identifier,
    Expression,
}

pub struct ParseError<'a> {
    pub expected: Option<TokenKind>,
    pub got: Option<WithSpan<'a, Token<'a>>>,
}

impl<'a> Debug for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut slices = vec![];
        let mut footer = vec![];
        let got_label = self
            .got
            .map(|got| format!("Unexpected token: {:?}", got.item()))
            .unwrap_or("".to_owned());
        let expected_label = self
            .expected
            .map(|expected| format!("Expected: {:?}", expected))
            .unwrap_or("".to_owned());
        match (self.got, self.expected) {
            (None, None) => {}
            (None, Some(_expected)) => {
                footer.push(Annotation {
                    id: None,
                    label: Some(&expected_label),
                    annotation_type: AnnotationType::Help,
                });
            }
            (Some(got), None) => {
                let got_span = got.span();
                let loc = got.span().location_offset();
                slices.push(Slice {
                    source: got_span.extra,
                    line_start: 0,
                    origin: None,
                    annotations: vec![SourceAnnotation {
                        label: &got_label,
                        range: (loc, loc + 1),
                        annotation_type: AnnotationType::Error,
                    }],
                    fold: true,
                })
            }
            (Some(got), Some(_expected)) => {
                let got_span = got.span();
                let loc = got.span().location_offset();
                slices.push(Slice {
                    source: got_span.extra,
                    line_start: 0,
                    origin: None,
                    annotations: vec![SourceAnnotation {
                        label: &expected_label,
                        range: (loc, loc + 1),
                        annotation_type: AnnotationType::Error,
                    }],
                    fold: true,
                })
            }
        }
        let snippet = Snippet {
            title: Some(Annotation {
                label: Some("Parsing Error"),
                id: None,
                annotation_type: AnnotationType::Error,
            }),
            footer,
            slices,
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        };
        writeln!(f, "\n{}", DisplayList::from(snippet))
    }
}

impl<'a> nom::error::ParseError<Tokens<'a>> for ParseError<'a> {
    fn from_error_kind(input: Tokens<'a>, kind: ErrorKind) -> Self {
        Self {
            expected: None,
            got: Some(input.tok[0]),
        }
    }

    fn append(input: Tokens<'a>, kind: ErrorKind, other: Self) -> Self {
        Self {
            expected: None,
            got: Some(input.tok[0]),
        }
    }
}

pub struct LexError<'a> {
    pub expected: Option<&'a str>,
    pub got: Span<'a>,
}

impl<'a> Debug for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(expected) = self.expected {
            write!(
                f,
                "Lexing Error ({}:{}): At `{:?}`, expected `{:?}`",
                self.got.location_line(),
                self.got.get_utf8_column(),
                self.got.fragment(),
                expected
            )
        } else {
            write!(
                f,
                "Lexing Error ({}:{}): At `{:?}`",
                self.got.location_line(),
                self.got.get_utf8_column(),
                self.got.fragment(),
            )
        }
    }
}

impl<'a> nom::error::ParseError<Span<'a>> for LexError<'a> {
    fn from_error_kind(input: Span<'a>, kind: ErrorKind) -> Self {
        Self {
            expected: None,
            got: input,
        }
    }

    fn append(input: Span<'a>, kind: ErrorKind, other: Self) -> Self {
        Self {
            expected: None,
            got: input,
        }
    }
}

pub type LexResult<'a, I, O> = nom::IResult<I, O, LexError<'a>>;
pub type ParseResult<'a, I, O> = nom::IResult<I, O, ParseError<'a>>;

fn main() {
    println!("Hello, world!");
}
