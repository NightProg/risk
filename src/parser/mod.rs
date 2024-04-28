use std::ops::Range;
use chumsky::combinator::MapWithSpan;
use chumsky::Parser;
use chumsky::prelude::*;
use chumsky::primitive::Choice;

use crate::ast::Statment;
use crate::ast::TypeAssign;
use crate::ast::TypeDecl;
use crate::ast::Variant;
use crate::ast::{App, Bind, BinOp, Bool, Expr, Identifier, Literal, Pattern, Type};
use crate::ast::LiteralKind;
use crate::ast::Program;

fn tuple_to_float((a, (_, b)): (String, (char, String))) -> f64 {
    format!("{}.{}", a, b).parse().unwrap()
}

pub trait RiskParser<R> = Parser<char, R, Error = Simple<char>>;


pub fn parser() ->  impl RiskParser<Statment> {
    let literal = literal_parser_kind()
        .map_with_span(|x, span| Expr::Literal(Literal { lit: x, span: span.into() }));

    // let keyword = choice((
    //     text::keyword("let"),
    //     text::keyword("in"),
    //     text::keyword("if"),
    //     text::keyword("then"),
    //     text::keyword("else"),
    //     text::keyword("match"),
    //     text::keyword("with"),
    //     ));

    let ident = text::ident()
        .map_with_span(|id, span: Range<usize>| {
            Identifier { name: id, span: span.into() }
        });

    let pc_id = filter(
        |x: &char| x.is_uppercase()
    ).then(
        filter(
            |x: &char| x.is_alphanumeric()
        ).repeated()
    ).map_with_span(|x, span: Range<usize>| {
        let mut id = String::from(x.0);
        let rest: String = x.1.iter().collect();
        id.push_str(&rest);

        Identifier { name: id, span: span.into() }
    });


    let type_ = recursive(|type_| {
        let type_ident = ident
            .clone()
            .map(
                |id| Type::Generic(id)
            );

        let type_pc_id = pc_id
            .map(
                |id| Type::Id(id)
            );

        let type_app = pc_id
            .then(type_.clone().repeated())
            .map_with_span(|(id, ty), span: Range<usize>| {
                Type::App(id, ty, span.into())
            });

        let type_group =
            type_
                .clone()
                .delimited_by(just("{"), just("}"));

        let type_tuple = type_
            .clone()
            .then_ignore(just(','))
            .repeated()
            .delimited_by(just("("), just(")"))
            .map_with_span(|x, span: Range<usize>| Type::Tuple(x, span.into()))
        ;

        let type_function = type_
            .clone()
            .then_ignore(just("->"))
            .repeated()
            .then(type_)
            .map_with_span(|(a, b), span: Range<usize>| Type::Func(Box::new(b), a, span.into()));

        choice((type_ident, type_pc_id, type_app, type_group, type_tuple, type_function))


    });


    let pattern = recursive(|pat| {
        let pattern_wildcard = just('_')
            .map_with_span(|_, span: Range<usize>| Pattern::Wildcard(span.into()));

        let pattern_ident = ident.clone().map(
            |id| Pattern::Variable(id)
        );

        let pattern_pc_id = pc_id.map(
            |id| Pattern::Id(id)
        );

        let pattern_app = pc_id
            .then(type_.clone().repeated())
            .map_with_span(|(id, pat), span: Range<usize>| {
                Pattern::App(id, pat, span.into())
            });

        let pattern_list = pat
            .clone()
            .then(just(':'))
            .then(pat)
            .map_with_span(|((a, _), b), span: Range<usize>| {
                Pattern::ListCons(Box::new(a), Box::new(b), span.into())
            });

        let pattern_literal = literal_parser_kind()
            .map_with_span(|x, span| Pattern::Literal(Literal { lit: x, span: span.into() }));

        choice((pattern_wildcard, pattern_ident, pattern_pc_id, pattern_app, pattern_list, pattern_literal))
    });


    let ident_pc_id = choice((ident.clone(), pc_id));



    let expr = recursive(|expr| {
        let expr_ident = ident.clone()
            .map(
                |id| {
                    Expr::Identifier(id)
                }
            );

        let expr_pc_id = pc_id
            .map(
                |id| {
                    Expr::Id(id)
                }
            );

        let expr_literal = literal;

        let expr_bind = ident
            .clone()
            .then(
                pattern.clone().repeated()
            )
            .then_ignore(just('='))
            .then(expr.clone())
            .map_with_span(|((id, pattern), expr), span: Range<usize>| {
                Bind::new(id, pattern, expr, span.into())
            });

        let expr_let = text::keyword("let")
            .then(
                expr_bind
                    .then_ignore(just(';'))
                    .repeated()
            )
            .then_ignore(text::keyword("in"))
            .then(expr.clone())
            .map_with_span(|((_, binds), expr), span: Range<usize>| {
                Expr::Let(binds, Box::new(expr), span.into())
            });

        let expr_annotation = expr
            .clone()
            .then_ignore(just("::"))
            .then(type_.clone())
            .map_with_span(|(expr, ty), span: Range<usize>| {
                Expr::Ann(Box::new(expr), ty, span.into())
            });
        //
        // let op = just("+")
        //     .or(just("-"))
        //     .or(just("*"))
        //     .or(just("/"))
        //     .or(just("%"))
        //     .or(just("<"))
        //     .or(just(">"))
        //     .or(just("=="))
        //     .or(just("!="))
        //     .or(just("<="))
        //     .or(just(">="))
        //     .or(just("&&"))
        //     .map(|a| match a {
        //         "+" => BinOp::Add,
        //         "-" => BinOp::Sub,
        //         "*" => BinOp::Mul,
        //         "/" => BinOp::Div,
        //         "%" => BinOp::Mod,
        //         "<" => BinOp::LessThan,
        //         ">" => BinOp::GreaterThan,
        //         "==" => BinOp::Eq,
        //         "!=" => BinOp::Ineq,
        //         "<=" => BinOp::LessThanOrEq,
        //         ">=" => BinOp::GreaterThanOrEq,
        //         "&&" => BinOp::And,
        //         _ => unreachable!()
        //
        //     });
        //
        // let expr_binop = expr
        //     .clone()
        //     .then(
        //         op
        //     )
        //     .then(expr.clone())
        //     .map_with_span(|((a, op), b), span: Range<usize>| {
        //         Expr::BinOp(op, Box::new(a), Box::new(b), span.into())
        //     });

        let expr_condition = text::keyword("if")
            .then(expr.clone())
            .then_ignore(text::keyword("then"))
            .then(expr.clone())
            .then_ignore(text::keyword("else"))
            .then(expr.clone())
            .map_with_span(|(((_, b), c), d), span: Range<usize>| {
                Expr::Condition(Box::new(b), Box::new(c), Box::new(d), span.into())
            });

        let expr_app = ident_pc_id
            .then(expr.clone().repeated())
            .map_with_span(|(id, exprs), span: Range<usize>| {
                Expr::App(App::new(id, exprs, span.into()))
            });

        let expr_match = text::keyword("match")
            .then(expr.clone())
            .then_ignore(text::keyword("in"))
            .then(
                just("|")
                    .then(pattern.clone())
                    .then_ignore(just("->"))
                    .then(expr.clone())
                    .repeated()
            )
            .map_with_span(|((_, expr), arms), span: Range<usize>| {
                Expr::Match(Box::new(expr), arms
                    .iter()
                    .map(|a| (a.0.1.clone(), a.1.clone()))
                    .map(|(a, b)| (Box::new(a), Box::new(b)))
                    .collect(), span.into())
            });

        let expr_lambda  = just('\\')
            .then(pattern.clone().repeated())
            .then_ignore(just("->"))
            .then(expr)
            .map_with_span(|((_, pat), expr), span: Range<usize>| {
                Expr::Lambda(pat, Box::new(expr), span.into())
            });

        choice((expr_ident, expr_pc_id, expr_literal, expr_let, expr_annotation, expr_condition, expr_app, expr_match, expr_lambda))
    });

    let bind = ident
        .clone()
        .then(
            pattern.repeated()
        )
        .then_ignore(just('='))
        .then(expr)
        .map_with_span(|((id, pattern), expr), span: Range<usize>| {
            Bind::new(id, pattern, expr, span.into())
        });

    let variant = pc_id
        .then(
            type_.clone().repeated()
        )
        .map_with_span(|(id, types), span: Range<usize>| {
            Variant::new(id, types, span.into())
        });

    let type_decl = text::keyword("type")
        .then(pc_id)
        .then(ident.clone().repeated())
        .then_ignore(just('='))
        .then(variant.
                then_ignore(just('|'))
                .repeated()
        )
        .map_with_span(|(((_, id), typevars), variants), s| {
            TypeDecl::new(id, typevars, variants, s.into())
        });

    let type_assign = ident.clone()
        .then_ignore(just("::"))
        .then(type_)
        .map_with_span(|(id, ty), s| {
            TypeAssign::new(id, ty, s.into())
        });

    let stmt = choice((type_decl.map(Statment::TypeDecl), type_assign.map(Statment::TypeAssign), bind.map(Statment::Bind)));

    // let program =
    //     stmt.then_ignore(just(';')).repeated().map(Program::new);

    stmt



}
pub fn literal_parser_kind() -> impl RiskParser<LiteralKind>  {
    let int = text::int(10)
        .map(|x: String| LiteralKind::Integer(x.parse().unwrap()));

    let float = text::int(10)
        .then(
            just('.').then(text::int(10))
        )
        .map(|x| LiteralKind::Float(
            tuple_to_float(x)
        ));

    let string = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map(|x| LiteralKind::String(x.iter().collect()));


    let char = just('\'')
        .ignore_then(none_of('\''))
        .then_ignore(just('\''))
        .map(|x| LiteralKind::Char(x));

    let true_bool = just("true")
        .map(|_| LiteralKind::Bool(Bool::True));

    let false_bool = just("false")
        .map(|_| LiteralKind::Bool(Bool::False));



    choice((float, int, string, char, true_bool, false_bool))
}
