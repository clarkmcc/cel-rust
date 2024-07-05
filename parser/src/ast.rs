use std::sync::Arc;

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
    FunctionCall(Box<Expression>, Option<Box<Expression>>, Vec<Expression>),

    List(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),

    Atom(Atom),
    Ident(Arc<String>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Member {
    Attribute(Arc<String>),
    Index(Box<Expression>),
    Fields(Vec<(Arc<String>, Expression)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Int(i64),
    UInt(u64),
    Float(f64),
    String(Arc<String>),
    Bytes(Arc<Vec<u8>>),
    Bool(bool),
    Null,
}
