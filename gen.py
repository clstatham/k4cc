PUNCTUATORS = {
    "OBrack":    "[",
    "CBrack":    "]",
    "OParen":    "(",
    "CParen":    ")",
    "OBrace":    "{",
    "CBrace":    "}",
    "Period":    ".",
    "RArrow":    "->",
    "PlusPlus":  "++",
    "MinusMinus":"--",
    "Ampersand": "&",
    "Star":      "*",
    "Plus":      "+",
    "Minus":     "-",
    "Tilde":     "~",
    "Bang":      "!",
    "FSlash":    "/",
    "Percent":   "%",
    "LtLt":      "<<",
    "GtGt":      ">>",
    "Lt":        "<",
    "Gt":        ">",
    "LtEq":      "<=",
    "GtEq":      ">=",
    "EqEq":      "==",
    "BangEq":    "!=",
    "Caret":     "^",
    "Bar":       "|",
    "AndAnd":    "&&",
    "BarBar":    "||",
    "Question":  "?",
    "Colon":     ":",
    "Semicolon": ";",
    "Ellipsis":  "...",
    "Equals":    "=",
    "StarEq":    "*=",
    "SlashEq":   "/=",
    "PercentEq": "%=",
    "PlusEq":    "+=",
    "MinusEq":   "-=",
    "LtLtEq":    "<<=",
    "GtGtEq":    ">>=",
    "AndEq":     "&=",
    "CaretEq":   "^=",
    "BarEq":     "|=",
    "Comma":     ",",
    "Hash":      "#",
    "HashHash":  "##",
}

KEYWORDS = [
    "Auto",
    "Break",
    "Case",
    "Char",
    "Const",
    "Continue",
    "Default",
    "Do",
    "Double",
    "Else",
    "Enum",
    "Extern",
    "Float",
    "For",
    "Goto",
    "If",
    "Inline",
    "Int",
    "Long",
    "Register",
    "Restrict",
    "Return",
    "Short",
    "Signed",
    "Sizeof",
    "Static",
    "Struct",
    "Switch",
    "Typedef",
    "Union",
    "Unsigned",
    "Void",
    "Volatile",
    "While",
]

def gen_punctuators_enum():
    out = """#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Punctuator {
"""
    for key in PUNCTUATORS.keys():
        out += "    " + key + ",\n"
    out += "}\n"
    return out

def gen_puncutators_syntax():
    out = ""
    for key, val in PUNCTUATORS.items():
        out += "syntax! { punc_" + key.lower() + ", \"" + val + "\", Punctuator, " + key + " }\n"
    return out

def gen_keywords_enum():
    out = """#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
"""
    for key in KEYWORDS:
        out += "    " + key + ",\n"
    out += "}\n"
    return out

def gen_keywords_syntax():
    out = ""
    for key in KEYWORDS:
        out += "syntax! { kw_" + key.lower() + ", \"" + key.lower() + "\", Keyword, " + key + " }\n"
    return out

def gen_keywords_lexer():
    out = """pub fn lex_keyword(inp: Span<'_>) -> IResult<Span<'_>, WithSpan<'_, Token<'_>>> {
    alt((alt(("""
    for i in range(len(KEYWORDS)):
        key = KEYWORDS[i].lower()
        if i % 20 == 19:
            out += ")), alt(("
        out += "kw_" + key + ",\n"
    out += """    ))))(inp)
}
"""
    return out

def gen_punctuators_lexer():
    out = """pub fn lex_punctuator(inp: Span<'_>) -> IResult<Span<'_>, WithSpan<'_, Token<'_>>> {
    alt((alt(("""
    puncs = list(PUNCTUATORS.keys())
    for i in range(len(puncs)):
        key = puncs[i].lower()
        if i % 20 == 19:
            out += ")), alt(("
        out += "punc_" + key + ",\n"
    out += """    ))))(inp)
}
"""
    return out


def gen_puncutators_parser():
    out = ""
    for key in PUNCTUATORS.keys():
        out += "tag_token! { punc_" + key.lower() + ", Punctuator, " + key + " }\n"
    return out

def gen_keywords_parser():
    out = ""
    for key in KEYWORDS:
        out += "tag_token! { kw_" + key.lower() + ", Keyword, " + key + " }\n"
    return out

if __name__ == "__main__":
    with open("src/lexer/gen.rs", "w") as f:
        print("""//! Auto-generated by `gen.py`
//! Do not modify!

use super::{Span, Token, WithSpan};
use nom::branch::*;
use nom::bytes::complete::*;
use nom::combinator::*;
use crate::IResult;

// This is inspired / taken from `monkey-rust`
// https://github.com/Rydgel/monkey-rust/blob/22976ecf97f6b3aa007ba2b511fc9539d7940e13/lib/lexer/mod.rs#L16
macro_rules! syntax {
    ($func:ident, $tag:literal, $typ:ident, $tok:ident) => {
        pub fn $func(s: Span<'_>) -> IResult<Span<'_>, WithSpan<'_, Token<'_>>> {
            map(tag($tag), |s| WithSpan(s, Token::$typ($typ::$tok)))(s)
        }
    };
}

""", file=f)
        print(gen_keywords_enum(), file=f)
        print(gen_punctuators_enum(), file=f)
        print(gen_keywords_syntax(), file=f)
        print(gen_puncutators_syntax(), file=f)
        print(gen_keywords_lexer(), file=f)
        print(gen_punctuators_lexer(), file=f)
    
    with open("src/parser/gen.rs", "w") as f:
        print("""//! Auto-generated by `gen.py`
//! Do not modify!

use crate::lexer::{
    gen::{Keyword, Punctuator},
    Token, Tokens, WithSpan,
};
use nom::bytes::complete::*;
use nom::combinator::*;
use crate::IResult;

// This is inspired / taken from `monkey-rust`
// https://github.com/Rydgel/monkey-rust/blob/22976ecf97f6b3aa007ba2b511fc9539d7940e13/lib/parser/mod.rs#L15C20-L15C20
macro_rules! tag_token {
    ($func:ident, $typ:ident, $tok:ident) => {
        pub fn $func(tokens: Tokens<'_>) -> IResult<Tokens<'_>, Tokens<'_>> {
            verify(take(1usize), |t: &Tokens<'_>| {
                matches!(t.tok[0], WithSpan(_, Token::$typ($typ::$tok)))
            })(tokens)
        }
    };
}
""", file=f)
        print(gen_puncutators_parser(), file=f)
        print(gen_keywords_parser(), file=f)
