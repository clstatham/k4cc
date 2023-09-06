use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::{multispace0, one_of},
    combinator::*,
    error::ErrorKind,
    error_position,
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    Err,
};

use crate::{
    lexer::{gen::Punctuator, Token, Tokens, WithSpan},
    ParseError, ParseResult, TokenKind,
};

use self::ast::{
    BlockItem, CompoundStatement, Constant, Declaration, Expr, ExternalDeclaration,
    FunctionDefinition, Ident, InfixExpr, InfixOp, JumpStatement, LabeledStatement, Precedence,
    Statement, TranslationUnit, TypeSpecifier, TypedIdent,
};
use self::gen::*;

pub mod ast;
pub mod gen;

pub fn parse_ident(inp: Tokens) -> ParseResult<Tokens, WithSpan<Ident>> {
    // expect_next!(inp, Token::Ident(id), "ident", |span, rest| {
    //     Ok((rest, WithSpan(span, Ident(id.0))))
    // });
    let (rest, one) = take(1usize)(inp)?;
    if one.tok.is_empty() {
        Err(Err::Error(ParseError {
            expected: Some(TokenKind::Identifier),
            got: None,
        }))
    } else {
        match one.tok[0] {
            WithSpan(span, Token::Ident(id)) => Ok((rest, WithSpan(span, Ident(id.0)))),
            tok => Err(Err::Error(crate::ParseError {
                expected: Some(TokenKind::Identifier),
                got: Some(tok),
            })),
        }
    }
}

pub fn parse_constant(inp: Tokens) -> ParseResult<Tokens, WithSpan<Constant>> {
    let (rest, one) = take(1usize)(inp)?;
    if one.tok.is_empty() {
        Err(Err::Error(ParseError {
            expected: Some(TokenKind::Constant),
            got: None,
        }))
    } else {
        match one.tok[0] {
            WithSpan(span, Token::Constant(val)) => match val {
                crate::lexer::Constant::Integer(val) => {
                    Ok((rest, WithSpan(span, Constant::Integer(val))))
                }
                // crate::lexer::Constant::Character(val) => Ok((
                //     rest,
                //     Expr::Constant(WithSpan(span, Constant::Character(val))),
                // )),
            },
            tok => Err(Err::Error(crate::ParseError {
                expected: Some(TokenKind::Constant),
                got: Some(tok),
            })),
        }
    }
}

pub fn parse_infix_op<'a>(inp: WithSpan<'a, Token<'a>>) -> (Precedence, Option<WithSpan<InfixOp>>) {
    match inp.item() {
        Token::Punctuator(Punctuator::Plus) => {
            (Precedence::Sum, Some(WithSpan(inp.span(), InfixOp::Plus)))
        }
        Token::Punctuator(Punctuator::Minus) => {
            (Precedence::Sum, Some(WithSpan(inp.span(), InfixOp::Minus)))
        }
        Token::Punctuator(Punctuator::Star) => (
            Precedence::Product,
            Some(WithSpan(inp.span(), InfixOp::Star)),
        ),
        Token::Punctuator(Punctuator::FSlash) => (
            Precedence::Product,
            Some(WithSpan(inp.span(), InfixOp::FSlash)),
        ),
        _ => (Precedence::Lowest, None),
    }
}

pub fn parse_type_specifier(inp: Tokens) -> ParseResult<Tokens, WithSpan<TypeSpecifier>> {
    // context(
    // "type specifier",
    alt(
        (
            map(kw_void, |s| s.first_into(TypeSpecifier::Void)),
            map(kw_char, |s| s.first_into(TypeSpecifier::Char)),
            map(kw_short, |s| s.first_into(TypeSpecifier::Short)),
            map(kw_int, |s| s.first_into(TypeSpecifier::Int)),
            map(kw_long, |s| s.first_into(TypeSpecifier::Long)),
            map(parse_ident, |s| {
                WithSpan(s.span(), TypeSpecifier::TypedefName(*s.item()))
            }),
        ), // )),
    )(inp)
}

pub fn parse_typed_ident(inp: Tokens) -> ParseResult<Tokens, WithSpan<TypedIdent>> {
    map(tuple((parse_type_specifier, parse_ident)), |(typ, id)| {
        WithSpan(typ.span(), TypedIdent { typ, id })
    })(inp)
}

pub fn parse_expr(inp: Tokens) -> ParseResult<Tokens, WithSpan<Expr>> {
    parse_pratt_expr(inp, Precedence::Lowest)
}

pub fn parse_atom_expr(inp: Tokens) -> ParseResult<Tokens, WithSpan<Expr>> {
    alt((
        map(parse_constant, |con| {
            WithSpan(con.span(), Expr::Constant(con))
        }),
        map(parse_ident, |id| WithSpan(id.span(), Expr::Ident(id))),
        delimited(punc_oparen, parse_expr, punc_cparen),
    ))(inp)
}

pub fn parse_pratt_expr(
    inp: Tokens,
    precedence: Precedence,
) -> ParseResult<Tokens, WithSpan<Expr>> {
    let (rest, left) = parse_atom_expr(inp)?;
    parse_pratt_expr_impl(rest, precedence, left)
}

fn parse_pratt_expr_impl<'a>(
    inp: Tokens<'a>,
    precedence: Precedence,
    left: WithSpan<'a, Expr<'a>>,
) -> ParseResult<'a, Tokens<'a>, WithSpan<'a, Expr<'a>>> {
    let (rest1, one) = take(1usize)(inp)?;
    if one.tok.is_empty() {
        Ok((rest1, left))
    } else {
        let preview = one.tok[0];
        let (p_prec, _op) = parse_infix_op(preview);
        match p_prec {
            p_prec if precedence < p_prec => {
                let (rest2, left2) = parse_infix_expr(inp, left)?;
                parse_pratt_expr_impl(rest2, precedence, left2)
            }
            _ => Ok((inp, left)),
        }
    }
}

pub fn parse_infix_expr<'a>(
    inp: Tokens<'a>,
    left: WithSpan<'a, Expr<'a>>,
) -> ParseResult<'a, Tokens<'a>, WithSpan<'a, Expr<'a>>> {
    let (rest1, one) = take(1usize)(inp)?;
    if one.tok.is_empty() {
        Err(Err::Error(ParseError {
            expected: Some(TokenKind::Expression),
            got: None,
        }))
    } else {
        let next = one.tok[0];
        let (precedence, op) = parse_infix_op(next);
        match op {
            Some(op) => {
                let (rest2, right) = parse_pratt_expr(rest1, precedence)?;
                Ok((
                    rest2,
                    WithSpan(
                        left.span(),
                        Expr::Infix(Box::new(WithSpan(
                            left.span(),
                            InfixExpr {
                                op,
                                lhs: left,
                                rhs: right,
                            },
                        ))),
                    ),
                ))
            }
            None => Err(Err::Error(ParseError {
                expected: Some(TokenKind::Expression),
                got: Some(next),
            })),
        }
    }
}

pub fn parse_declaration(inp: Tokens) -> ParseResult<Tokens, WithSpan<Declaration>> {
    map(tuple((parse_typed_ident, parse_expr)), |(id, expr)| {
        WithSpan(id.span(), Declaration { id, expr })
    })(inp)
}

pub fn parse_jump_statement(inp: Tokens) -> ParseResult<Tokens, WithSpan<JumpStatement>> {
    alt((
        map(delimited(kw_goto, parse_ident, punc_semicolon), |id| {
            WithSpan(id.span(), JumpStatement::Goto(id))
        }),
        map(
            tuple((kw_return, opt(parse_expr), punc_semicolon)),
            |(ret, expr, _)| WithSpan(ret.first_span(), JumpStatement::Return(expr)),
        ),
    ))(inp)
}

pub fn parse_block_item(inp: Tokens) -> ParseResult<Tokens, WithSpan<BlockItem>> {
    alt((
        map(parse_declaration, |decl| {
            WithSpan(decl.span(), BlockItem::Declaration(decl))
        }),
        map(parse_statement, |stmt| {
            WithSpan(stmt.span(), BlockItem::Statement(stmt))
        }),
    ))(inp)
}

pub fn parse_compound_statement(inp: Tokens) -> ParseResult<Tokens, WithSpan<CompoundStatement>> {
    map(
        tuple((punc_obrace, many0(parse_block_item), punc_cbrace)),
        |(ob, items, _)| WithSpan(ob.first_span(), CompoundStatement(items)),
    )(inp)
}

pub fn parse_labeled_statement(inp: Tokens) -> ParseResult<Tokens, WithSpan<LabeledStatement>> {
    map(
        tuple((parse_ident, punc_colon, parse_statement)),
        |(label, _, stmt)| WithSpan(label.span(), LabeledStatement { label, stmt }),
    )(inp)
}

pub fn parse_statement(inp: Tokens) -> ParseResult<Tokens, WithSpan<Statement>> {
    alt((
        map(parse_labeled_statement, |stmt| {
            WithSpan(stmt.span(), Statement::Labeled(Box::new(stmt)))
        }),
        map(parse_compound_statement, |stmt| {
            WithSpan(stmt.span(), Statement::Compound(stmt))
        }),
        map(parse_jump_statement, |stmt| {
            WithSpan(stmt.span(), Statement::Jump(stmt))
        }),
        map(terminated(parse_expr, punc_semicolon), |stmt| {
            WithSpan(stmt.span(), Statement::Expr(stmt))
        }),
    ))(inp)
}

pub fn parse_function_definition(inp: Tokens) -> ParseResult<Tokens, WithSpan<FunctionDefinition>> {
    map(
        tuple((
            parse_typed_ident,
            delimited(
                punc_oparen,
                separated_list0(punc_comma, parse_typed_ident),
                punc_cparen,
            ),
            parse_compound_statement,
        )),
        |(id, param_list, body)| {
            WithSpan(
                id.span(),
                FunctionDefinition {
                    id,
                    param_list,
                    body,
                },
            )
        },
    )(inp)
}

pub fn parse_external_declaration(
    inp: Tokens,
) -> ParseResult<Tokens, WithSpan<ExternalDeclaration>> {
    alt((
        map(parse_function_definition, |f| {
            WithSpan(f.span(), ExternalDeclaration::FunctionDefinition(f))
        }),
        map(parse_declaration, |decl| {
            WithSpan(decl.span(), ExternalDeclaration::Declaration(decl))
        }),
    ))(inp)
}

pub fn parse_translation_unit(inp: Tokens) -> ParseResult<Tokens, WithSpan<TranslationUnit>> {
    map(many0(parse_external_declaration), |decls| {
        WithSpan(inp.first_span(), TranslationUnit(decls))
    })(inp)
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{lex_tokens, Span, Token, Tokens},
        parser::parse_translation_unit,
    };

    #[test]
    fn test_parse_1() {
        let inp = "
int main() {
    return 1 + 2;
}";
        let (garbage, toks) = lex_tokens(Span::new_extra(inp, inp)).unwrap();
        assert!(garbage.is_empty());
        let (garbage, tu) = parse_translation_unit(Tokens::new(&toks)).unwrap();
        assert!(
            garbage.tok.len() == 1 && garbage.tok[0].item() == &Token::EOI,
            "{:#?}",
            garbage
        );
        dbg!(tu);
    }
}
