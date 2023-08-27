use crate::ast::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};
use chumsky::prelude::*;
use chumsky::Parser;
use std::rc::Rc;

fn boolean() -> impl Parser<char, Expression, Error = Simple<char>> {
    just("true")
        .to(true)
        .or(just("false").to(false))
        .map(|b| Expression::Atom(Atom::Bool(b)))
}

/// Parses floating point and integer numbers and returns them as [`Expr::Atom(Atom::Double(...))`]
/// or [`Expr::Atom(Atom::Int(...))`] types. The following formats are supported:
/// - `1`
/// - `1.`
/// - `1.0`
/// - `-1`
/// - `-1.0`
/// - `1e10`
/// - `1e-10`
/// - `1E10`
/// - `1E-10`
/// - `-1e10`
/// - `1u`
fn numbers() -> impl Parser<char, Expression, Error = Simple<char>> {
    let digits = text::digits::<char, Simple<char>>(10);

    let frac = just('.').chain::<char, _, _>(digits.clone().or_not());

    let exp = just('e')
        .or(just('E'))
        .chain::<char, _, _>(one_of("+-").or_not())
        .chain::<char, _, _>(digits.clone());

    let float_or_int = text::int::<char, Simple<char>>(10)
        .chain::<char, _, _>(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .try_map(|chars, span| {
            let str = chars.into_iter().collect::<String>();

            if let Ok(i) = str.parse::<i64>() {
                Ok(Expression::Atom(Atom::Int(i)))
            } else if let Ok(f) = str.parse::<f64>() {
                Ok(Expression::Atom(Atom::Float(f)))
            } else {
                Err(Simple::expected_input_found(span, None, None))
            }
        });

    let unsigned_integer = text::int::<char, Simple<char>>(10)
        .then_ignore(just('u'))
        .map(|s: String| Expression::Atom(Atom::UInt(s.as_str().parse().unwrap())));

    choice((unsigned_integer, float_or_int))
        .padded()
        .labelled("number")
}

fn str_inner(
    delimiter: &str,
    escaping: bool,
) -> impl Parser<char, String, Error = Simple<char>> + '_ {
    let unicode = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(4)
        .collect::<String>()
        .validate(|digits, span, emit| {
            char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                emit(Simple::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let hex_code_point = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(2)
        .collect::<String>()
        .validate(|digits, span, emit| {
            char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                emit(Simple::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let octal_code_point = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(3)
        .collect::<String>()
        .validate(|digits, span, emit| {
            char::from_u32(u32::from_str_radix(&digits, 8).unwrap()).unwrap_or_else(|| {
                emit(Simple::custom(span, "invalid unicode character"));
                '\u{FFFD}' // unicode replacement character
            })
        });

    let escape = just('\\').ignore_then(choice((
        just('\\'),
        just('/'),
        just('"'),
        just('b').to('\x08'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('u').ignore_then(unicode),
        just('x').or(just('X')).ignore_then(hex_code_point),
        octal_code_point,
    )));

    let mut forbidden = just(delimiter).boxed();
    let mut inner_string = forbidden.not().boxed();

    if escaping {
        forbidden = just(delimiter).or(just("\\")).boxed();
        inner_string = forbidden.not().or(escape).boxed();
    }

    inner_string
        .repeated()
        .delimited_by(just(delimiter), just(delimiter))
        .collect::<String>()
}

fn bytes_inner(delimiter: &str) -> impl Parser<char, Vec<u8>, Error = Simple<char>> + '_ {
    let hex_code_point = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .exactly(2)
        .collect::<String>()
        .validate(|digits, span, emit| {
            u8::from_str_radix(&digits, 16).unwrap_or_else(|_| {
                emit(Simple::custom(span, "invalid hexadecimal character"));
                0u8
            })
        });

    let octal_code_point = filter::<_, _, Simple<char>>(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(3)
        .collect::<String>()
        .validate(|digits, span, emit| {
            u8::from_str_radix(&digits, 8).unwrap_or_else(|_| {
                emit(Simple::custom(span, "invalid octal code point"));
                0u8
            })
        });

    let escape = just('\\')
        .ignore_then(choice((
            just('\\').to(b'\\'),
            just(delimiter).to(delimiter.as_bytes()[0]),
            just('n').to(b'\n'),
            just('a').to(b'\x07'),
            just('b').to(b'\x08'),
            just('f').to(b'\x0c'),
            just('r').to(b'\r'),
            just('t').to(b'\t'),
            just('v').to(b'\x0b'),
            just('x').or(just('X')).ignore_then(hex_code_point),
            octal_code_point,
        )))
        .map(|c: u8| vec![c]);

    let forbidden = just(delimiter).or(just("\\")).boxed();
    let not_forbidden = forbidden.not().map(|c: char| c.to_string().into_bytes());
    let inner_string = not_forbidden.or(escape).boxed();

    inner_string
        .repeated()
        .delimited_by(just(delimiter), just(delimiter))
        .collect::<Vec<Vec<u8>>>()
        .flatten()
}

// Ref https://github.com/01mf02/jaq/blob/main/jaq-parse/src/token.rs
// See also https://github.com/PRQL/prql/blob/main/prql-compiler/src/parser/lexer.rs#L295-L354
// A parser for strings; adapted from Chumsky's JSON example parser.
fn str_() -> impl Parser<char, Expression, Error = Simple<char>> {
    let single_quoted_string = str_inner("'", true).labelled("single quoted string");

    let double_quoted_string = str_inner("\"", true).labelled("double quoted string");

    // Byte literals
    let single_quoted_bytes = just("b")
        .ignore_then(bytes_inner("'"))
        .labelled("single quoted byte string");
    let double_quoted_bytes = just("b")
        .ignore_then(bytes_inner("\""))
        .labelled("single quoted byte string");

    // Raw strings don't interpret escape sequences.

    let single_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("'", false))
        .labelled("single quoted raw string");

    let double_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("\"", false))
        .labelled("double quoted raw string");

    let triple_single_quoted_raw_string = just("r")
        .or(just("R"))
        .ignore_then(str_inner("'''", false))
        .labelled("triple ' quoted string");

    let triple_single_quoted_escaped_string =
        str_inner("'''", true).labelled("triple ' quoted escaped string");

    let triple_double_quoted_string = str_inner("\"\"\"", true).labelled("triple \" quoted string");

    let strings = choice((
        triple_single_quoted_raw_string,
        triple_single_quoted_escaped_string,
        triple_double_quoted_string,
        single_quoted_raw_string,
        single_quoted_string,
        double_quoted_raw_string,
        double_quoted_string,
    ))
    .map(|s| Expression::Atom(Atom::String(s.into())));

    let bytes = choice((single_quoted_bytes, double_quoted_bytes))
        .map(|b| Expression::Atom(Atom::Bytes(b.into())));

    choice((strings, bytes))
}

pub fn parser() -> impl Parser<char, Expression, Error = Simple<char>> {
    let ident = text::ident::<char, Simple<char>>()
        .padded()
        .map(|name| Expression::Ident(Rc::new(name)))
        .labelled("identifier");

    let null = just("null")
        .padded()
        .map(|_| Expression::Atom(Atom::Null))
        .labelled("null");

    let literal = choice((numbers(), boolean(), str_(), null)).labelled("literal");

    let attribute_access = just('.').ignore_then(ident.clone()).map(|rhs| match rhs {
        Expression::Ident(name) => Box::new(Member::Attribute(name)),
        _ => panic!("Expected ident!"),
    });

    let expr = recursive(|expr| {
        let expr_in_paren = expr.clone().delimited_by(just('('), just(')'));

        let expr_list = expr
            .clone()
            .padded()
            .separated_by(just(','))
            .then_ignore(just(',').or_not())
            .collect::<Vec<_>>();

        let function_call = just('(')
            .ignore_then(expr_list.clone())
            .then_ignore(just(')'))
            .map(|args| Box::new(Member::FunctionCall(args)))
            .labelled("function call");

        let index_access = just('[')
            .ignore_then(expr.clone())
            .then_ignore(just(']'))
            .map(|arg: Expression| Box::new(Member::Index(Box::new(arg))))
            .labelled("index");

        let list = expr_list
            .clone()
            // Ignore trailing comma
            .delimited_by(just('['), just(']'))
            .map(Expression::List)
            .labelled("list");

        let map_item = expr
            .clone()
            .then_ignore(just(':'))
            .then(expr.clone())
            .padded()
            .labelled("map item");

        let map = map_item
            .clone()
            .separated_by(just(','))
            .delimited_by(just('{'), just('}'))
            .padded()
            .map(Expression::Map)
            .labelled("map");

        let field_identifier = text::ident::<char, Simple<char>>()
            .padded()
            .map(|s| {
                let ref_counted_field_id: Rc<String> = Rc::from(s);
                ref_counted_field_id
            })
            .labelled("field identifier");

        let field_item = field_identifier
            .clone()
            .then_ignore(just(':'))
            .then(expr.clone());

        let field_items = field_item
            .clone()
            .separated_by(just(','))
            .delimited_by(just('{'), just('}'))
            .padded()
            .labelled("field items");

        let field_inits = ident
            .clone()
            .then(just('.').ignore_then(ident.clone()).repeated())
            .foldl(|lhs: Expression, rhs: Expression| {
                // We convert the Ident Expressions to attribute member expressions except for the left most one
                // Ident(A), Ident(B) -> Member(Ident(A), Attribute(B))
                // Member(Ident(A), Attribute(B)), Ident(C) -> Member(Member(Ident(A), Attribute(B)), Attribute(C))
                match rhs {
                    Expression::Ident(name) => Expression::Member(
                        Box::new(lhs), // LHS stays as an Ident Expression
                        Box::new(Member::Attribute(name)),
                    ),
                    _ => panic!("Expected ident!"),
                }
            })
            .then(field_items)
            .map(|(lhs, items)| Expression::Member(Box::new(lhs), Box::new(Member::Fields(items))));

        let primary = choice((literal, field_inits, ident, expr_in_paren, list, map))
            .labelled("primary")
            .boxed();

        let member_chain = primary
            .clone()
            .then(
                choice((
                    attribute_access.clone(),
                    function_call.clone(),
                    index_access.clone(),
                ))
                .repeated(),
            )
            .map(|(lhs_expression, members)| {
                members.into_iter().fold(lhs_expression, |acc, member| {
                    Expression::Member(Box::new(acc), member)
                })
            })
            .labelled("member");

        let member = choice((member_chain, primary.clone()));

        let op = |c| just::<char, _, Simple<char>>(c).padded();

        let unary_op = op('!').to(UnaryOp::Not).or(op('-').to(UnaryOp::Neg));

        let not_or_negation = unary_op
            .repeated()
            .at_least(1)
            .then(member.clone())
            .foldr(|op, rhs: Expression| Expression::Unary(op, Box::new(rhs)))
            .labelled("unary");

        let unary = choice((not_or_negation, member.clone())).padded();

        let product_div_op = op('*')
            .to(ArithmeticOp::Multiply)
            .or(op('/').to(ArithmeticOp::Divide))
            .or(op('%').to(ArithmeticOp::Modulus));

        let multiplication = unary
            .clone()
            .then(product_div_op.then(unary.clone()).repeated())
            .foldl(|lhs, (binary_op, rhs)| {
                Expression::Arithmetic(Box::new(lhs), binary_op, Box::new(rhs))
            })
            .labelled("product_or_division");

        let sum_sub_op = op('+')
            .to(ArithmeticOp::Add)
            .or(op('-').to(ArithmeticOp::Subtract));

        let addition = multiplication
            .clone()
            .then(sum_sub_op.then(multiplication.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Arithmetic(Box::new(lhs), op, Box::new(rhs)))
            .labelled("sub_or_sub");

        let relationship_op = just("==")
            .to(RelationOp::Equals)
            .or(just("!=").to(RelationOp::NotEquals))
            .or(just(">=").to(RelationOp::GreaterThanEq))
            .or(just("<=").to(RelationOp::LessThanEq))
            .or(just('>').to(RelationOp::GreaterThan))
            .or(just('<').to(RelationOp::LessThan))
            .or(just("in").to(RelationOp::In));

        let relation = addition
            .clone()
            .then(relationship_op.then(addition.clone()).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Relation(Box::new(lhs), op, Box::new(rhs)))
            .labelled("comparison");

        let conditional_and = relation
            .clone()
            .then(just("&&").then(relation.clone()).repeated())
            .foldl(|lhs, (_op, rhs)| Expression::And(Box::new(lhs), Box::new(rhs)))
            .labelled("conditional and");

        let conditional_or = conditional_and
            .clone()
            .then(just("||").then(conditional_and.clone()).repeated())
            .foldl(|lhs, (_op, rhs)| Expression::Or(Box::new(lhs), Box::new(rhs)))
            .labelled("conditional or");

        let ternary = conditional_or
            .clone()
            .then(
                just("?")
                    .ignore_then(conditional_or.clone())
                    .then_ignore(just(":"))
                    .then(conditional_or.clone())
                    .or_not(),
            )
            .map(|(condition, ternary)| match ternary {
                Some((true_expression, false_expression)) => Expression::Ternary(
                    Box::new(condition),
                    Box::new(true_expression),
                    Box::new(false_expression),
                ),
                None => condition,
            })
            .labelled("ternary");

        ternary
    });

    expr.clone()
        .padded()
        .then_ignore(end())
        .labelled("expression")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_parser_unsigned_numbers() {
        //let unsigned_integer = text::int::<char, Simple<char>>(10).then_ignore(just('u')).map(|s: String| Expr::Atom(Atom::UInt(s.as_str().parse().unwrap())));
        //assert_eq!(unsigned_integer.parse("1u"), Ok(Expr::Atom(Atom::UInt(1))));
        assert_eq!(numbers().parse("1u"), Ok(Expression::Atom(Atom::UInt(1))));
        assert_eq!(numbers().parse("1up"), Ok(Expression::Atom(Atom::UInt(1))));
    }

    #[test]
    fn test_number_parser_int() {
        assert_eq!(numbers().parse("1"), Ok(Expression::Atom(Atom::Int(1))));

        // Debatable if this should be allowed. Ref CEL Spec:
        // https://github.com/google/cel-spec/blob/master/doc/langdef.md#numeric-values
        // "negative integers are produced by the unary negation operator"
        assert_eq!(numbers().parse("100"), Ok(Expression::Atom(Atom::Int(100))));
    }

    #[test]
    fn test_boolean_parser_errors() {
        assert!(boolean().parse("-true").is_err());
        assert!(boolean().parse("!1").is_err());
    }

    #[test]
    fn test_str_inner_parser() {
        // Taking the idea from
        // REF: https://github.com/PRQL/prql/blob/main/prql-compiler/src/parser/lexer.rs#L295

        let triple_single_quoted_escaped_string =
            str_inner("'''", true).labelled("triple ' quoted escaped string");

        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"''''''"),
            Ok(String::from(""))
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''hello'''"),
            Ok(String::from("hello"))
        );
        // Check triple quoted strings interpret escape sequences (note this is a rust raw string, not a CEL raw string)
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''\n'''"),
            Ok(String::from("\n"))
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''x''x'''"),
            Ok(String::from("x''x"))
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"''' '''"),
            Ok(String::from(" "))
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''\xFF'''"),
            Ok(String::from("ÿ"))
        );
        assert_eq!(
            triple_single_quoted_escaped_string.parse(r"'''\377'''"),
            Ok(String::from("ÿ"))
        );
    }

    #[test]
    fn test_str_parser() {
        assert_eq!(
            str_().parse("'Hello!'"),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse("\"Hello!\""),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse("'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\n").into())))
        );
        assert_eq!(
            str_().parse(r"'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\n").into())))
        );

        assert_eq!(
            str_().parse(r"'''hello'''"),
            Ok(Expression::Atom(Atom::String(String::from("hello").into())))
        );
        // Check triple quoted strings interpret escape sequences (note this is a rust raw string, not a CEL raw string)
        assert_eq!(
            str_().parse(r"'''\n'''"),
            Ok(Expression::Atom(Atom::String(String::from("\n").into())))
        );
    }

    #[test]
    fn test_raw_str_parser() {
        assert_eq!(
            str_().parse(r"r'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\\n").into())))
        );
        assert_eq!(
            str_().parse(r"R'\n'"),
            Ok(Expression::Atom(Atom::String(String::from("\\n").into())))
        );
        assert_eq!(
            str_().parse("r'1'"),
            Ok(Expression::Atom(Atom::String(String::from("1").into())))
        );
        assert_eq!(
            str_().parse("r\"Hello!\""),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse("R\"Hello!\""),
            Ok(Expression::Atom(Atom::String(
                String::from("Hello!").into()
            )))
        );
        assert_eq!(
            str_().parse(r"r'''hello'''"),
            Ok(Expression::Atom(Atom::String(String::from("hello").into())))
        );
        assert_eq!(
            str_().parse(r"r'''\n'''"),
            Ok(Expression::Atom(Atom::String(String::from("\\n").into())))
        );
    }

    #[test]
    fn test_raw_bytes_simple() {
        let expected: Vec<u8> = vec![97, 98, 99];

        assert_eq!(
            str_().parse(r"b'abc'"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }

    #[test]
    fn test_raw_bytes_escaped_newlines() {
        let expected: Vec<u8> = vec![10];

        assert_eq!(
            str_().parse(r"b'\n'"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }

    #[test]
    fn test_raw_bytes_escaped_delimiter() {
        let expected: Vec<u8> = vec![39];

        assert_eq!(
            str_().parse(r"b'\''"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }

    #[test]
    fn test_raw_bytes_unicode() {
        let expected: Vec<u8> = vec![195, 191];

        assert_eq!(
            str_().parse(r"b'ÿ'"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }

    #[test]
    fn test_raw_bytes_invalid_utf8() {
        let expected: Vec<u8> = vec![0, 255];

        assert_eq!(
            str_().parse(r"b'\000\xff'"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }

    #[test]
    fn test_raw_bytes_unicode_as_octal_escaped() {
        let expected: Vec<u8> = vec![195, 191];

        assert_eq!(
            str_().parse(r"b'\303\277'"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }

    #[test]
    fn test_raw_bytes_single_octal() {
        let expected = vec![0xffu8];

        assert_eq!(
            str_().parse(r"b'\377'"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }

    #[test]
    fn test_raw_bytes_single_hexadecimal() {
        let expected = vec![0xffu8];

        assert_eq!(
            str_().parse(r"b'\xFF'"),
            Ok(Expression::Atom(Atom::Bytes(expected.into())))
        );
    }
}
