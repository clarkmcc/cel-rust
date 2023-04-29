use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum RelationOp {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,
    In,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulus,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnaryOp {
    Not,
    DoubleNot,
    Minus,
    DoubleMinus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Arithmetic(Box<Expression>, ArithmeticOp, Box<Expression>),
    Relation(Box<Expression>, RelationOp, Box<Expression>),

    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),

    Member(Box<Expression>, Box<Member>),

    List(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),

    Atom(Atom),
    Ident(Rc<String>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Member {
    Attribute(Rc<String>),
    FunctionCall(Vec<Expression>),
    Index(Box<Expression>),
    Fields(Vec<(Rc<String>, Expression)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Int(i32),
    UInt(u32),
    Float(f64),
    String(Rc<String>),
    Bytes(Rc<Vec<u8>>),
    Bool(bool),
    Null,
}

#[cfg(test)]
mod tests {
    use crate::parser::ExpressionParser;
    use crate::{Atom::*, Expression, Expression::*, Member::*};

    fn parse(input: &str) -> Expression {
        ExpressionParser::new()
            .parse(input)
            .unwrap_or_else(|e| panic!("{}", e))
    }

    fn assert_parse_eq(input: &str, expected: Expression) {
        assert_eq!(parse(input), expected);
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
    fn single_quote_str() {
        assert_parse_eq("'foobar'", Atom(String("foobar".to_string().into())))
    }

    #[test]
    fn double_quote_str() {
        assert_parse_eq(r#""foobar""#, Atom(String("foobar".to_string().into())))
    }

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
        assert_parse_eq(
            r#"'foobar'"#,
            Atom(String("foobar".to_string().into()).into()),
        );
        println!("{:?}", parse(r#"1 == '1'"#))
    }

    #[test]
    fn test_parse_map_macro() {
        println!("{:?}", parse("[1, 2, 3].map(x, x * 2)"))
    }

    #[test]
    fn nested_attributes() {
        assert_parse_eq(
            "a.b[1]",
            Member(
                Member(
                    Ident("a".to_string().into()).into(),
                    Attribute("b".to_string().into()).into(),
                )
                .into(),
                Index(Atom(Int(1)).into()).into(),
            ),
        )
    }
}
