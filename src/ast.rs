use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(clippy::all)] pub parser, "/cel.rs");

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
pub enum Expression<'a> {
    Arithmetic(Box<Expression<'a>>, ArithmeticOp, Box<Expression<'a>>),
    Relation(Box<Expression<'a>>, RelationOp, Box<Expression<'a>>),

    Ternary(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),
    Or(Box<Expression<'a>>, Box<Expression<'a>>),
    And(Box<Expression<'a>>, Box<Expression<'a>>),
    Unary(UnaryOp, Box<Expression<'a>>),

    Member(Box<Expression<'a>>, Box<Member<'a>>),

    CallFunction(Vec<Expression<'a>>),
    List(Vec<Expression<'a>>),
    Map(Vec<(Expression<'a>, Expression<'a>)>),

    Atom(Atom<'a>),
    Ident(&'a str),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Member<'a> {
    Attribute(&'a str),
    FunctionCall(Vec<Expression<'a>>),
    Index(Box<Expression<'a>>),
    Fields(Vec<(&'a str, Expression<'a>)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom<'a> {
    Int(i32),
    UInt(u32),
    Float(f64),
    String(String),
    Bytes(&'a [u8]),
    Bool(bool),
    Null,
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression;
    use crate::ast::{parser::ExpressionParser, Atom::*, Expression::*, Member::*};

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
    fn nested_attributes() {
        assert_parse_eq(
            "a.b[1]",
            Member(
                Member(Ident("a".into()).into(), Attribute("b".into()).into()).into(),
                Index(Atom(Int(1)).into()).into(),
            )
            .into(),
        )
    }
}
