use crate::{ArithmeticOp, Atom, Expression, Member, RelationOp};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use chumsky::Parser;
use std::collections::HashMap;

/// Returns a parser that parses singly and doubly quoted strings with possible escape sequences
/// and returns them as [`Expression::Atom(Atom::String(...))`] types. The following formats are
/// supported:
/// - `"hello world"`
/// - `'hello world'`
/// - `"hello \"world\""`
/// - `'hello \'world\''`
fn strings<'a>() -> impl Parser<'a, &'a str, Expression, extra::Err<Rich<'a, char>>> {
    let escape = just('\\')
        .then(choice((
            just('\\'),
            just('/'),
            just('"'),
            just('b').to('\x08'),
            just('f').to('\x0c'),
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
            just('u').ignore_then(text::digits(16).exactly(4).slice().validate(
                |digits, span, emitter| {
                    char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(|| {
                        emitter.emit(Rich::custom(span, "invalid unicode character"));
                        '\u{FFFD}' // unicode replacement character
                    })
                },
            )),
        )))
        .ignored()
        .boxed();

    let single_quoted_string = none_of("'")
        .ignored()
        .or(escape.clone())
        .repeated()
        .slice()
        .map(ToString::to_string)
        .delimited_by(just('\''), just('\''))
        .padded()
        .boxed();
    let double_quoted_string = none_of("\\\"")
        .ignored()
        .or(escape)
        .repeated()
        .slice()
        .map(ToString::to_string)
        .delimited_by(just('"'), just('"'))
        .padded()
        .boxed();
    choice((single_quoted_string, double_quoted_string))
        .map(|s| Expression::Atom(Atom::String(s.into())))
}

/// Parses floating point and integer numbers and returns them as [`Expression::Atom(Atom::Float(...))`]
/// or [`Expression::Atom(Atom::Int(...))`] types. The following formats are supported:
/// - `1`
/// - `1.0`
/// - `-1`
/// - `-1.0`
/// - `1e10`
/// - `1e-10`
/// - `1E10`
/// - `1E-10`
/// - `-1e10`
fn numbers<'a>() -> impl Parser<'a, &'a str, Expression, extra::Err<Rich<'a, char>>> {
    let digits = text::digits(10).slice();
    let frac = just('.').then(digits);
    let exp = just('e')
        .or(just('E'))
        .then(one_of("+-").or_not())
        .then(digits);
    let floating = just('-')
        .or_not()
        .then(text::int(10))
        .then(frac)
        .then(exp.or_not())
        .map_slice(|s: &str| Expression::Atom(Atom::Float(s.parse().unwrap())))
        .boxed();
    let integer = text::int(10)
        .slice()
        .map(|s: &str| Expression::Atom(Atom::Int(s.parse().unwrap())));
    choice((floating, integer)).padded()
}

/// Parses boolean values and returns them as [`Expression::Atom(Atom::Bool(...))`] types.
fn booleans<'a>() -> impl Parser<'a, &'a str, Expression, extra::Err<Rich<'a, char>>> {
    let true_ = just("true").map(|_| Expression::Atom(Atom::Bool(true)));
    let false_ = just("false").map(|_| Expression::Atom(Atom::Bool(false)));
    choice((true_, false_)).padded()
}

/// Parses identifiers and returns them as [`Expression::Ident(...)`] types.
fn identifiers<'a>() -> impl Parser<'a, &'a str, Expression, extra::Err<Rich<'a, char>>> {
    text::ident()
        .slice()
        .map(|s: &str| Expression::Ident(s.to_string().into()))
}

fn nulls<'a>() -> impl Parser<'a, &'a str, Expression, extra::Err<Rich<'a, char>>> {
    just("null").to(Expression::Atom(Atom::Null))
}

fn parser<'a>() -> impl Parser<'a, &'a str, Expression, extra::Err<Rich<'a, char>>> {
    let identifiers = identifiers().boxed();
    let strings = strings().boxed();
    let numbers = numbers().boxed();
    let booleans = booleans().boxed();
    let null = nulls().boxed();

    recursive(|expr| {
        // Lists
        let items = expr.clone().separated_by(just(',')).collect::<Vec<_>>();
        let list = items
            .clone()
            .delimited_by(just('['), just(']'))
            .map(Expression::List);

        let atom = choice((
            null.clone(),
            booleans.clone(),
            identifiers.clone(),
            strings.clone(),
            numbers.clone(),
        ))
        .or(list.clone())
        .or(expr.clone())
        .padded()
        .boxed();

        // Operations
        let op = choice((
            just('/').to(ArithmeticOp::Divide),
            just('*').to(ArithmeticOp::Multiply),
        ));
        let product = atom.clone().foldl(
            op.then(atom.clone()).repeated(),
            |lhs: Expression, (op, rhs)| Expression::Arithmetic(lhs.into(), op, rhs.into()),
        );
        let op = choice((
            just('+').to(ArithmeticOp::Add),
            just('-').to(ArithmeticOp::Subtract),
        ));
        let sum = product
            .clone()
            .foldl(op.then(product.clone()).repeated(), |lhs, (op, rhs)| {
                Expression::Arithmetic(lhs.into(), op, rhs.into())
            });
        let op = choice((
            just("==").to(RelationOp::Equals),
            just("!=").to(RelationOp::NotEquals),
            just("<").to(RelationOp::LessThan),
            just("<=").to(RelationOp::LessThanEq),
            just(">").to(RelationOp::GreaterThan),
            just(">=").to(RelationOp::GreaterThanEq),
            just("in").to(RelationOp::In),
        ));
        let compare = sum
            .clone()
            .foldl(op.then(sum.clone()).repeated(), |lhs, (op, rhs)| {
                Expression::Relation(lhs.into(), op, rhs.into())
            });

        // Maps
        let keys = choice((
            booleans.clone(),
            identifiers.clone(),
            numbers.clone(),
            strings.clone(),
        ));
        let map = keys
            .then(just(':'))
            .then(booleans.clone())
            .separated_by(just(','))
            .collect::<Vec<_>>()
            .delimited_by(just('{'), just('}'))
            .map(|entries| {
                entries
                    .into_iter()
                    .map(|((key, _), value)| (key, value))
                    .collect::<Vec<_>>()
            })
            .map(Expression::Map);

        choice((compare, map, atom)).padded()
    })
}

#[test]
fn test() {
    // test_expr(
    //     "{1:true}",
    //     Some(Expression::Map(vec![(
    //         Expression::Atom(Atom::Int(1)),
    //         Expression::Atom(Atom::Bool(true)),
    //     )])),
    // );
    test_expr(
        "1 < 2",
        Some(Expression::Relation(
            Expression::Atom(Atom::Int(1)).into(),
            RelationOp::LessThan,
            Expression::Atom(Atom::Int(2)).into(),
        )),
    );
    test_expr(
        "[1, 2] + [3, 4]",
        Some(Expression::Arithmetic(
            Expression::List(vec![
                Expression::Atom(Atom::Int(1)),
                Expression::Atom(Atom::Int(2)),
            ])
            .into(),
            ArithmeticOp::Add,
            Expression::List(vec![
                Expression::Atom(Atom::Int(3)),
                Expression::Atom(Atom::Int(4)),
            ])
            .into(),
        )),
    );
    test_expr(
        "[1]",
        Some(Expression::List(vec![Expression::Atom(Atom::Int(1))])),
    );
    test_expr(
        "[[1]]",
        Some(Expression::List(vec![Expression::List(vec![
            Expression::Atom(Atom::Int(1)),
        ])])),
    );
    test_expr("true", Some(Expression::Atom(Atom::Bool(true))));
    test_expr("false", Some(Expression::Atom(Atom::Bool(false))));
    test_expr("null", Some(Expression::Atom(Atom::Null)));
    test_expr("foo", Some(Expression::Ident("foo".to_string().into())));
    test_expr("10.5", Some(Expression::Atom(Atom::Float(10.5))));
    test_expr("10", Some(Expression::Atom(Atom::Int(10))));
    test_expr(
        "'foobar'",
        Some(Expression::Atom(Atom::String("foobar".to_string().into()))),
    );
}

fn test_expr(script: &str, expect: Option<Expression>) {
    let (out, errors) = parser().parse(script).into_output_errors();
    errors.iter().for_each(|e| {
        Report::build(ReportKind::Error, (), e.span().start)
            .with_message(e.to_string())
            .with_label(
                Label::new(e.span().into_range())
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(script))
            .unwrap()
    });
    match expect {
        None => assert_eq!(errors.len(), 0),
        Some(expr) => assert_eq!(expr, out.unwrap()),
    }
}
