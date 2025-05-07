use lalrpop_util::lalrpop_mod;

pub mod ast;
pub use ast::*;

pub mod parse;
pub use parse::*;

pub mod error;
pub use error::ParseError;

lalrpop_mod!(#[allow(clippy::all)] pub parser, "/cel.rs");

/// Parses a CEL expression and returns it.
///
/// # Example
/// ```
/// use cel_parser::parse;
/// let expr = parse("1 + 1").unwrap();
/// println!("{:?}", expr);
/// ```
pub fn parse(input: &str) -> Result<Spanned<Expression>, ParseError> {
    // Wrap the internal parser function - whether larlpop or chumsky

    // Example for a possible new chumsky based parser...
    // parser().parse(input)
    //     .into_result()
    //     .map_err(|e|  {
    //         ParseError {
    //             msg: e.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("\n")
    //         }
    //     })

    // Existing Larlpop Parser:
    crate::parser::SpannedExpressionParser::new()
        .parse(input)
        .map_err(|e| ParseError::from_lalrpop(input, e))
}

#[cfg(test)]
mod tests {
    use crate::{
        ArithmeticOp,
        Atom::{self, *},
        Expression::{self, *},
        Member::*,
        RelationOp, SpanExtension, Spanned, UnaryOp,
    };

    pub fn parse(input: &str) -> Spanned<Expression> {
        crate::parse(input).unwrap_or_else(|e| panic!("{}", e))
    }

    fn assert_parse_eq(input: &str, expected: Expression) {
        assert_eq!(parse(input).inner, expected);
    }

    #[test]
    fn ident() {
        assert_parse_eq("a", Ident("a".to_string().unspanned().into()));
        assert_parse_eq("hello ", Ident("hello".to_string().unspanned().into()));
    }

    #[test]
    fn simple_int() {
        assert_parse_eq("1", Atom(Int(1)))
    }

    #[test]
    fn simple_float() {
        assert_parse_eq("1.0", Atom(Float(1.0)))
    }

    #[test]
    fn other_floats() {
        assert_parse_eq("1e3", Expression::Atom(Atom::Float(1000.0)));
        assert_parse_eq("1e-3", Expression::Atom(Atom::Float(0.001)));
        assert_parse_eq("1.4e-3", Expression::Atom(Atom::Float(0.0014)));
    }

    #[test]
    fn single_quote_str() {
        assert_parse_eq("'foobar'", Atom(String("foobar".to_string().into())))
    }

    #[test]
    fn double_quote_str() {
        assert_parse_eq(r#""foobar""#, Atom(String("foobar".to_string().into())))
    }

    // #[test]
    // fn single_quote_raw_str() {
    //     assert_parse_eq(
    //         "r'\n'",
    //         ExpressionInner::Atom(String("\n".to_string().into())),
    //     );
    // }

    #[test]
    fn single_quote_bytes() {
        assert_parse_eq("b'foo'", Atom(Bytes(b"foo".to_vec().into())));
        assert_parse_eq("b''", Atom(Bytes(b"".to_vec().into())));
    }

    #[test]
    fn double_quote_bytes() {
        assert_parse_eq(r#"b"foo""#, Atom(Bytes(b"foo".to_vec().into())));
        assert_parse_eq(r#"b"""#, Atom(Bytes(b"".to_vec().into())));
    }

    #[test]
    fn bools() {
        assert_parse_eq("true", Atom(Bool(true)));
        assert_parse_eq("false", Atom(Bool(false)));
    }

    #[test]
    fn nulls() {
        assert_parse_eq("null", Atom(Null));
    }

    #[test]
    fn structure() {
        println!("{:+?}", parse("{1 + a: 3}"));
    }

    #[test]
    fn simple_str() {
        assert_parse_eq(r#"'foobar'"#, Atom(String("foobar".to_string().into())));
        println!("{:?}", parse(r#"1 == '1'"#))
    }

    #[test]
    fn test_parse_map_macro() {
        assert_parse_eq(
            "[1, 2, 3].map(x, x * 2)",
            FunctionCall(
                Box::new(Ident("map".to_string().unspanned().into()).unspanned()),
                Some(Box::new(
                    List(vec![
                        Atom(Int(1)).unspanned(),
                        Atom(Int(2)).unspanned(),
                        Atom(Int(3)).unspanned(),
                    ])
                    .unspanned(),
                )),
                vec![
                    Ident("x".to_string().unspanned().into()).unspanned(),
                    Arithmetic(
                        Box::new(Ident("x".to_string().unspanned().into()).unspanned()),
                        ArithmeticOp::Multiply.unspanned(),
                        Box::new(Atom(Int(2)).unspanned()),
                    )
                    .unspanned(),
                ],
            ),
        )
    }

    #[test]
    fn nested_attributes() {
        assert_parse_eq(
            "a.b[1]",
            Member(
                Member(
                    Ident("a".to_string().unspanned().into()).unspanned().into(),
                    Attribute("b".to_string().unspanned().into()).into(),
                )
                .unspanned()
                .into(),
                Index(Atom(Int(1)).unspanned().into()).into(),
            ),
        )
    }

    #[test]
    fn function_call_no_args() {
        assert_parse_eq(
            "a()",
            FunctionCall(
                Box::new(Ident("a".to_string().unspanned().into()).unspanned()),
                None,
                vec![],
            ),
        );
    }

    #[test]
    fn test_parser_bool_unary_ops() {
        assert_parse_eq(
            "!false",
            Unary(
                UnaryOp::Not.unspanned(),
                Box::new(Expression::Atom(Atom::Bool(false)).unspanned()),
            ),
        );
        assert_parse_eq(
            "!true",
            Unary(
                UnaryOp::Not.unspanned(),
                Box::new(Expression::Atom(Atom::Bool(true)).unspanned()),
            ),
        );
    }

    #[test]
    fn test_parser_binary_bool_expressions() {
        assert_parse_eq(
            "true == true",
            Relation(
                Box::new(Expression::Atom(Atom::Bool(true)).unspanned()),
                RelationOp::Equals.unspanned(),
                Box::new(Expression::Atom(Atom::Bool(true)).unspanned()),
            ),
        );
    }

    #[test]
    fn test_parser_bool_unary_ops_repeated() {
        assert_eq!(
            parse("!!true"),
            (Unary(
                UnaryOp::DoubleNot.unspanned(),
                Box::new(Expression::Atom(Atom::Bool(true)).unspanned()),
            )
            .unspanned())
        );
    }

    #[test]
    fn delimited_expressions() {
        assert_parse_eq(
            "(-((1)))",
            Unary(
                UnaryOp::Minus.unspanned(),
                Box::new(Expression::Atom(Atom::Int(1)).unspanned()),
            ),
        );
    }

    #[test]
    fn test_empty_list_parsing() {
        assert_eq!(parse("[]"), (List(vec![]).unspanned()));
    }

    #[test]
    fn test_int_list_parsing() {
        assert_parse_eq(
            "[1,2,3]",
            List(vec![
                Expression::Atom(Atom::Int(1)).unspanned(),
                Expression::Atom(Atom::Int(2)).unspanned(),
                Expression::Atom(Atom::Int(3)).unspanned(),
            ]),
        );
    }

    #[test]
    fn list_index_parsing() {
        assert_parse_eq(
            "[1,2,3][0]",
            Member(
                Box::new(
                    List(vec![
                        Expression::Atom(Int(1)).unspanned(),
                        Expression::Atom(Int(2)).unspanned(),
                        Expression::Atom(Int(3)).unspanned(),
                    ])
                    .unspanned(),
                ),
                Box::new(Index(Box::new(Expression::Atom(Int(0)).unspanned()))),
            ),
        );
    }

    #[test]
    fn mixed_type_list() {
        assert_parse_eq(
            "['0', 1, 3.0, null]",
            //"['0', 1, 2u, 3.0, null]",
            List(vec![
                Expression::Atom(String("0".to_string().into())).unspanned(),
                Expression::Atom(Int(1)).unspanned(),
                //ExpressiInExpressionInneron::Atom(UInt(2)).into_expression(),
                Expression::Atom(Float(3.0)).unspanned(),
                Expression::Atom(Null).unspanned(),
            ]),
        );
    }

    #[test]
    fn test_nested_list_parsing() {
        assert_parse_eq(
            "[[], [], [[1]]]",
            List(vec![
                List(vec![]).unspanned(),
                List(vec![]).unspanned(),
                List(vec![
                    List(vec![Expression::Atom(Int(1)).unspanned()]).unspanned()
                ])
                .unspanned(),
            ]),
        );
    }

    #[test]
    fn test_in_list_relation() {
        assert_parse_eq(
            "2 in [2]",
            Relation(
                Box::new(Expression::Atom(Int(2)).unspanned()),
                RelationOp::In.unspanned(),
                Box::new(List(vec![Expression::Atom(Int(2)).unspanned()]).unspanned()),
            ),
        );
    }

    #[test]
    fn test_empty_map_parsing() {
        assert_eq!(parse("{}"), (Map(vec![]).unspanned()));
    }

    #[test]
    fn test_nonempty_map_parsing() {
        assert_parse_eq(
            "{'a': 1, 'b': 2}",
            Map(vec![
                (
                    Expression::Atom(String("a".to_string().into())).unspanned(),
                    Expression::Atom(Int(1)).unspanned(),
                ),
                (
                    Expression::Atom(String("b".to_string().into())).unspanned(),
                    Expression::Atom(Int(2)).unspanned(),
                ),
            ]),
        );
    }

    #[test]
    fn nonempty_map_index_parsing() {
        assert_parse_eq(
            "{'a': 1, 'b': 2}[0]",
            Member(
                Box::new(
                    Map(vec![
                        (
                            Expression::Atom(String("a".to_string().into())).unspanned(),
                            Expression::Atom(Int(1)).unspanned(),
                        ),
                        (
                            Expression::Atom(String("b".to_string().into())).unspanned(),
                            Expression::Atom(Int(2)).unspanned(),
                        ),
                    ])
                    .unspanned(),
                ),
                Box::new(Index(Box::new(Expression::Atom(Int(0)).unspanned()))),
            ),
        );
    }

    #[test]
    fn integer_relations() {
        assert_parse_eq(
            "2 != 3",
            Relation(
                Box::new(Expression::Atom(Int(2)).unspanned()),
                RelationOp::NotEquals.unspanned(),
                Box::new(Expression::Atom(Int(3)).unspanned()),
            ),
        );
        assert_parse_eq(
            "2 == 3",
            Relation(
                Box::new(Expression::Atom(Int(2)).unspanned()),
                RelationOp::Equals.unspanned(),
                Box::new(Expression::Atom(Int(3)).unspanned()),
            ),
        );

        assert_parse_eq(
            "2 < 3",
            Relation(
                Box::new(Expression::Atom(Int(2)).unspanned()),
                RelationOp::LessThan.unspanned(),
                Box::new(Expression::Atom(Int(3)).unspanned()),
            ),
        );

        assert_parse_eq(
            "2 <= 3",
            Relation(
                Box::new(Expression::Atom(Int(2)).unspanned()),
                RelationOp::LessThanEq.unspanned(),
                Box::new(Expression::Atom(Int(3)).unspanned()),
            ),
        );
    }

    #[test]
    fn binary_product_expressions() {
        assert_parse_eq(
            "2 * 3",
            Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2)).unspanned()),
                ArithmeticOp::Multiply.unspanned(),
                Box::new(Expression::Atom(Atom::Int(3)).unspanned()),
            ),
        );
    }

    // #[test]
    // fn binary_product_negated_expressions() {
    //     assert_parse_eq(
    //         "2 * -3",
    //         Arithmetic(
    //             Box::new(ExpressionInner::Atom(Atom::Int(2))),
    //             ArithmeticOp::Multiply,
    //             Box::new(Unary(
    //                 UnaryOp::Minus,
    //                 Box::new(ExpressionInner::Atom(Atom::Int(3))),
    //             )),
    //         ),
    //     );
    //
    //     assert_parse_eq(
    //         "2 / -3",
    //         Arithmetic(
    //             Box::new(ExpressionInner::Atom(Int(2))),
    //             ArithmeticOp::Divide,
    //             Box::new(Unary(
    //                 UnaryOp::Minus,
    //                 Box::new(ExpressionInner::Atom(Int(3))),
    //             )),
    //         ),
    //     );
    // }

    #[test]
    fn test_parser_sum_expressions() {
        assert_parse_eq(
            "2 + 3",
            Arithmetic(
                Box::new(Expression::Atom(Atom::Int(2)).unspanned()),
                ArithmeticOp::Add.unspanned(),
                Box::new(Expression::Atom(Atom::Int(3)).unspanned()),
            ),
        );

        // assert_parse_eq(
        //     "2 - -3",
        //     Arithmetic(
        //         Box::new(ExpressionInner::Atom(Atom::Int(2)).into_expression()),
        //         ArithmeticOp::Subtract,
        //         Box::new(Unary(
        //             UnaryOp::Minus,
        //             Box::new(ExpressionInner::Atom(Atom::Int(3)).into_expression()),
        //         )),
        //     ),
        // );
    }

    #[test]
    fn conditionals() {
        assert_parse_eq(
            "true && true",
            And(
                Box::new(Expression::Atom(Bool(true)).unspanned()),
                Box::new(Expression::Atom(Bool(true)).unspanned()),
            ),
        );
        assert_parse_eq(
            "false || true",
            Or(
                Box::new(Expression::Atom(Bool(false)).unspanned()),
                Box::new(Expression::Atom(Bool(true)).unspanned()),
            ),
        );
    }
    #[test]
    fn test_ternary_true_condition() {
        assert_parse_eq(
            "true ? 'result_true' : 'result_false'",
            Ternary(
                Box::new(Expression::Atom(Bool(true)).unspanned()),
                Box::new(Expression::Atom(String("result_true".to_string().into())).unspanned()),
                Box::new(Expression::Atom(String("result_false".to_string().into())).unspanned()),
            ),
        );

        assert_parse_eq(
            "true ? 100 : 200",
            Ternary(
                Box::new(Expression::Atom(Bool(true)).unspanned()),
                Box::new(Expression::Atom(Int(100)).unspanned()),
                Box::new(Expression::Atom(Int(200)).unspanned()),
            ),
        );
    }

    #[test]
    fn test_ternary_false_condition() {
        assert_parse_eq(
            "false ? 'result_true' : 'result_false'",
            Ternary(
                Box::new(Expression::Atom(Bool(false)).unspanned()),
                Box::new(Expression::Atom(String("result_true".to_string().into())).unspanned()),
                Box::new(Expression::Atom(String("result_false".to_string().into())).unspanned()),
            ),
        );
    }

    #[test]
    fn test_operator_precedence() {
        assert_parse_eq(
            "a && b == 'string'",
            And(
                Box::new(Ident("a".to_string().unspanned().into()).unspanned()),
                Box::new(
                    Relation(
                        Box::new(Ident("b".to_string().unspanned().into()).unspanned()),
                        RelationOp::Equals.unspanned(),
                        Box::new(Expression::Atom(String("string".to_string().into())).unspanned()),
                    )
                    .unspanned(),
                ),
            ),
        );
    }

    #[test]
    fn test_foobar() {
        println!("{:?}", parse("foo.bar.baz == 10 && size(requests) == 3"))
    }

    #[test]
    fn test_unrecognized_token_error() {
        let source = r#"
            account.balance == transaction.withdrawal
                || (account.overdraftProtection
                    account.overdraftLimit >= transaction.withdrawal  - account.balance)
        "#;

        let err = crate::parse(source).unwrap_err();

        assert_eq!(err.msg, "unrecognized token: 'account'");

        assert_eq!(err.span.start.as_ref().unwrap().line, 3);
        assert_eq!(err.span.start.as_ref().unwrap().column, 20);
        assert_eq!(err.span.end.as_ref().unwrap().line, 3);
        assert_eq!(err.span.end.as_ref().unwrap().column, 27);
    }

    #[test]
    fn test_unrecognized_eof_error() {
        let source = r#" "#;

        let err = crate::parse(source).unwrap_err();

        assert_eq!(err.msg, "unrecognized eof");

        assert_eq!(err.span.start.as_ref().unwrap().line, 0);
        assert_eq!(err.span.start.as_ref().unwrap().column, 0);
        assert_eq!(err.span.end.as_ref().unwrap().line, 0);
        assert_eq!(err.span.end.as_ref().unwrap().column, 0);
    }

    #[test]
    fn test_invalid_token_error() {
        let source = r#"
            account.balance == §
        "#;

        let err = crate::parse(source).unwrap_err();

        assert_eq!(err.msg, "invalid token");

        assert_eq!(err.span.start.as_ref().unwrap().line, 1);
        assert_eq!(err.span.start.as_ref().unwrap().column, 31);
        assert_eq!(err.span.end.as_ref().unwrap().line, 1);
        assert_eq!(err.span.end.as_ref().unwrap().column, 31);
    }
}

#[cfg(test)]
#[cfg(feature = "preserve_spans")]
mod span_tests {

    use crate::{Expression, Member};

    use super::tests::parse;

    macro_rules! assert_span {
        ($a:expr, $f:tt .. $t:tt) => {
            assert_eq!($a.span, Some($f..$t), "spans are not equal");
        };
    }

    macro_rules! assert_matches {
        ($expression:expr, $pattern:pat $(if $guard:expr)? $(,)? => $content:tt) => {
            if let $pattern = $expression {
                $content
            } else {
                panic!(
                    "{} does not match {}",
                    stringify!($expression),
                    stringify!($pattern)
                )
            }
        };
    }

    #[test]
    fn spanned_atoms() {
        assert_span!(parse("543"), 0..3);
        assert_span!(parse("  543  "), 2..5);
    }

    #[test]
    fn spanned_arithmatic() {
        assert_span!(parse("5 + 4"), 0..5);

        assert_matches!(parse("5 + 4").inner, Expression::Arithmetic(five, plus, four) => {
            assert_span!(five, 0..1);
            assert_span!(plus, 2..3);
            assert_span!(four, 4..5);
        });

        assert_matches!(parse("5+4").inner, Expression::Arithmetic(five, plus, four) => {
            assert_span!(five, 0..1);
            assert_span!(plus, 1..2);
            assert_span!(four, 2..3);
        });

        assert_matches!(parse("5 +     4").inner, Expression::Arithmetic(five, plus, four) => {
            assert_span!(five, 0..1);
            assert_span!(plus, 2..3);
            assert_span!(four, 8..9);
        });
    }

    #[test]
    fn spanned_relation() {
        assert_span!(parse("5 == 4"), 0..6);
    }

    #[test]
    fn spanned_ternary() {
        assert_matches!(parse("true ? 4 : 5").inner, Expression::Ternary(i, t, e) => {
                assert_span!(i, 0..4);
                assert_span!(t, 7..8);
                assert_span!(e, 11..12);
            }
        );
    }

    #[test]
    fn spanned_ident() {
        let ident = parse("my_ident");
        assert_span!(ident, 0..8);
        assert_matches!(ident.inner, Expression::Ident(my_ident) => {
            assert_span!(my_ident, 0..8);
        })
    }

    #[test]
    fn spanned_unary() {
        let ident = parse("-my_ident");
        assert_span!(ident, 0..9);
        assert_matches!(ident.inner, Expression::Unary(op, target) => {
            assert_span!(op, 0..1);
            assert_span!(target, 1..9);
        })
    }

    #[test]
    fn spanned_linebreak() {
        assert_span!(parse("5\n+\n4"), 0..5);
    }

    #[test]
    fn spanned_members_attribute() {
        let fields = parse("foo.bar");
        assert_span!(fields, 0..7);
        assert_matches!(fields.inner, Expression::Member(foo, bar) => {
            assert_span!(foo, 0..3);
            assert_matches!(*bar, Member::Attribute(f) => {
                assert_span!(f, 4..7);
            });
        })
    }

    #[test]
    fn spanned_members_index() {
        let fields = parse("foo[0]");
        assert_span!(fields, 0..6);
        assert_matches!(fields.inner, Expression::Member(foo, bar) => {
            assert_span!(foo, 0..3);
            assert_matches!(*bar, Member::Index(f) => {
                assert_span!(f, 4..5);
            });
        })
    }

    #[test]
    fn spanned_members_members() {
        let fields = parse("Class {foo: 5, bar: false}");
        assert_span!(fields, 0..26);
        assert_matches!(fields.inner, Expression::Member(class, bar) => {
            assert_span!(class, 0..5);
            assert_matches!(*bar, Member::Fields(fields) => {
                let (foo, five) = &fields[0];
                assert_span!(foo, 7..10);
                assert_span!(five, 12..13);

                let (bar, fals) = &fields[1];
                assert_span!(bar, 15..18);
                assert_span!(fals, 20..25);
            });
        })
    }
}
