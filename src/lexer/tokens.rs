//! Auto-generated by `gen.py`
//! Do not modify!

use super::{Span, Token};
use nom::branch::*;
use nom::bytes::complete::*;
use nom::combinator::*;
use nom::IResult;

// This is inspired / taken from `monkey-rust`
// https://github.com/Rydgel/monkey-rust/blob/22976ecf97f6b3aa007ba2b511fc9539d7940e13/lib/lexer/mod.rs#L16
macro_rules! syntax {
    ($func:ident, $tag:literal, $tok:expr) => {
        pub fn $func(s: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
            map(tag($tag), |_| $tok)(s)
        }
    };
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Punctuator {
    OBrack,
    CBrack,
    OParen,
    CParen,
    OBrace,
    CBrace,
    Period,
    RArrow,
    PlusPlus,
    MinusMinus,
    Ampersand,
    Star,
    Plus,
    Minus,
    Tilde,
    Bang,
    FSlash,
    Percent,
    LtLt,
    GtGt,
    Lt,
    Gt,
    LtEq,
    GtEq,
    EqEq,
    BangEq,
    Caret,
    Bar,
    AndAnd,
    BarBar,
    Question,
    Colon,
    Semicolon,
    Ellipsis,
    Equals,
    StarEq,
    SlashEq,
    PercentEq,
    PlusEq,
    MinusEq,
    LtLtEq,
    GtGtEq,
    AndEq,
    CaretEq,
    BarEq,
    Comma,
    Hash,
    HashHash,
}

syntax! { kw_auto, "auto", Token::Keyword(Keyword::Auto) }
syntax! { kw_break, "break", Token::Keyword(Keyword::Break) }
syntax! { kw_case, "case", Token::Keyword(Keyword::Case) }
syntax! { kw_char, "char", Token::Keyword(Keyword::Char) }
syntax! { kw_const, "const", Token::Keyword(Keyword::Const) }
syntax! { kw_continue, "continue", Token::Keyword(Keyword::Continue) }
syntax! { kw_default, "default", Token::Keyword(Keyword::Default) }
syntax! { kw_do, "do", Token::Keyword(Keyword::Do) }
syntax! { kw_double, "double", Token::Keyword(Keyword::Double) }
syntax! { kw_else, "else", Token::Keyword(Keyword::Else) }
syntax! { kw_enum, "enum", Token::Keyword(Keyword::Enum) }
syntax! { kw_extern, "extern", Token::Keyword(Keyword::Extern) }
syntax! { kw_float, "float", Token::Keyword(Keyword::Float) }
syntax! { kw_for, "for", Token::Keyword(Keyword::For) }
syntax! { kw_goto, "goto", Token::Keyword(Keyword::Goto) }
syntax! { kw_if, "if", Token::Keyword(Keyword::If) }
syntax! { kw_inline, "inline", Token::Keyword(Keyword::Inline) }
syntax! { kw_int, "int", Token::Keyword(Keyword::Int) }
syntax! { kw_long, "long", Token::Keyword(Keyword::Long) }
syntax! { kw_register, "register", Token::Keyword(Keyword::Register) }
syntax! { kw_restrict, "restrict", Token::Keyword(Keyword::Restrict) }
syntax! { kw_return, "return", Token::Keyword(Keyword::Return) }
syntax! { kw_short, "short", Token::Keyword(Keyword::Short) }
syntax! { kw_signed, "signed", Token::Keyword(Keyword::Signed) }
syntax! { kw_sizeof, "sizeof", Token::Keyword(Keyword::Sizeof) }
syntax! { kw_static, "static", Token::Keyword(Keyword::Static) }
syntax! { kw_struct, "struct", Token::Keyword(Keyword::Struct) }
syntax! { kw_switch, "switch", Token::Keyword(Keyword::Switch) }
syntax! { kw_typedef, "typedef", Token::Keyword(Keyword::Typedef) }
syntax! { kw_union, "union", Token::Keyword(Keyword::Union) }
syntax! { kw_unsigned, "unsigned", Token::Keyword(Keyword::Unsigned) }
syntax! { kw_void, "void", Token::Keyword(Keyword::Void) }
syntax! { kw_volatile, "volatile", Token::Keyword(Keyword::Volatile) }
syntax! { kw_while, "while", Token::Keyword(Keyword::While) }

syntax! { punc_obrack, "[", Token::Punctuator(Punctuator::OBrack) }
syntax! { punc_cbrack, "]", Token::Punctuator(Punctuator::CBrack) }
syntax! { punc_oparen, "(", Token::Punctuator(Punctuator::OParen) }
syntax! { punc_cparen, ")", Token::Punctuator(Punctuator::CParen) }
syntax! { punc_obrace, "{", Token::Punctuator(Punctuator::OBrace) }
syntax! { punc_cbrace, "}", Token::Punctuator(Punctuator::CBrace) }
syntax! { punc_period, ".", Token::Punctuator(Punctuator::Period) }
syntax! { punc_rarrow, "->", Token::Punctuator(Punctuator::RArrow) }
syntax! { punc_plusplus, "++", Token::Punctuator(Punctuator::PlusPlus) }
syntax! { punc_minusminus, "--", Token::Punctuator(Punctuator::MinusMinus) }
syntax! { punc_ampersand, "&", Token::Punctuator(Punctuator::Ampersand) }
syntax! { punc_star, "*", Token::Punctuator(Punctuator::Star) }
syntax! { punc_plus, "+", Token::Punctuator(Punctuator::Plus) }
syntax! { punc_minus, "-", Token::Punctuator(Punctuator::Minus) }
syntax! { punc_tilde, "~", Token::Punctuator(Punctuator::Tilde) }
syntax! { punc_bang, "!", Token::Punctuator(Punctuator::Bang) }
syntax! { punc_fslash, "/", Token::Punctuator(Punctuator::FSlash) }
syntax! { punc_percent, "%", Token::Punctuator(Punctuator::Percent) }
syntax! { punc_ltlt, "<<", Token::Punctuator(Punctuator::LtLt) }
syntax! { punc_gtgt, ">>", Token::Punctuator(Punctuator::GtGt) }
syntax! { punc_lt, "<", Token::Punctuator(Punctuator::Lt) }
syntax! { punc_gt, ">", Token::Punctuator(Punctuator::Gt) }
syntax! { punc_lteq, "<=", Token::Punctuator(Punctuator::LtEq) }
syntax! { punc_gteq, ">=", Token::Punctuator(Punctuator::GtEq) }
syntax! { punc_eqeq, "==", Token::Punctuator(Punctuator::EqEq) }
syntax! { punc_bangeq, "!=", Token::Punctuator(Punctuator::BangEq) }
syntax! { punc_caret, "^", Token::Punctuator(Punctuator::Caret) }
syntax! { punc_bar, "|", Token::Punctuator(Punctuator::Bar) }
syntax! { punc_andand, "&&", Token::Punctuator(Punctuator::AndAnd) }
syntax! { punc_barbar, "||", Token::Punctuator(Punctuator::BarBar) }
syntax! { punc_question, "?", Token::Punctuator(Punctuator::Question) }
syntax! { punc_colon, ":", Token::Punctuator(Punctuator::Colon) }
syntax! { punc_semicolon, ";", Token::Punctuator(Punctuator::Semicolon) }
syntax! { punc_ellipsis, "...", Token::Punctuator(Punctuator::Ellipsis) }
syntax! { punc_equals, "=", Token::Punctuator(Punctuator::Equals) }
syntax! { punc_stareq, "*=", Token::Punctuator(Punctuator::StarEq) }
syntax! { punc_slasheq, "/=", Token::Punctuator(Punctuator::SlashEq) }
syntax! { punc_percenteq, "%=", Token::Punctuator(Punctuator::PercentEq) }
syntax! { punc_pluseq, "+=", Token::Punctuator(Punctuator::PlusEq) }
syntax! { punc_minuseq, "-=", Token::Punctuator(Punctuator::MinusEq) }
syntax! { punc_ltlteq, "<<=", Token::Punctuator(Punctuator::LtLtEq) }
syntax! { punc_gtgteq, ">>=", Token::Punctuator(Punctuator::GtGtEq) }
syntax! { punc_andeq, "&=", Token::Punctuator(Punctuator::AndEq) }
syntax! { punc_careteq, "^=", Token::Punctuator(Punctuator::CaretEq) }
syntax! { punc_bareq, "|=", Token::Punctuator(Punctuator::BarEq) }
syntax! { punc_comma, ",", Token::Punctuator(Punctuator::Comma) }
syntax! { punc_hash, "#", Token::Punctuator(Punctuator::Hash) }
syntax! { punc_hashhash, "##", Token::Punctuator(Punctuator::HashHash) }

pub fn lex_keyword(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    alt((
        alt((
            kw_auto,
            kw_break,
            kw_case,
            kw_char,
            kw_const,
            kw_continue,
            kw_default,
            kw_do,
            kw_double,
            kw_else,
            kw_enum,
            kw_extern,
            kw_float,
            kw_for,
            kw_goto,
            kw_if,
            kw_inline,
            kw_int,
            kw_long,
        )),
        alt((
            kw_register,
            kw_restrict,
            kw_return,
            kw_short,
            kw_signed,
            kw_sizeof,
            kw_static,
            kw_struct,
            kw_switch,
            kw_typedef,
            kw_union,
            kw_unsigned,
            kw_void,
            kw_volatile,
            kw_while,
        )),
    ))(inp)
}

pub fn lex_punctuator(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    alt((
        alt((
            punc_obrack,
            punc_cbrack,
            punc_oparen,
            punc_cparen,
            punc_obrace,
            punc_cbrace,
            punc_period,
            punc_rarrow,
            punc_plusplus,
            punc_minusminus,
            punc_ampersand,
            punc_star,
            punc_plus,
            punc_minus,
            punc_tilde,
            punc_bang,
            punc_fslash,
            punc_percent,
            punc_ltlt,
        )),
        alt((
            punc_gtgt,
            punc_lt,
            punc_gt,
            punc_lteq,
            punc_gteq,
            punc_eqeq,
            punc_bangeq,
            punc_caret,
            punc_bar,
            punc_andand,
            punc_barbar,
            punc_question,
            punc_colon,
            punc_semicolon,
            punc_ellipsis,
            punc_equals,
            punc_stareq,
            punc_slasheq,
            punc_percenteq,
            punc_pluseq,
        )),
        alt((
            punc_minuseq,
            punc_ltlteq,
            punc_gtgteq,
            punc_andeq,
            punc_careteq,
            punc_bareq,
            punc_comma,
            punc_hash,
            punc_hashhash,
        )),
    ))(inp)
}
