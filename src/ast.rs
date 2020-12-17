#[derive(Debug)]
pub enum RelationOp {
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,
    In,
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulus,
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    DoubleNot,
    Minus,
    DoubleMinus,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Arithmetic { left: &'a Expression<'a>, op: ArithmeticOp, right: &'a Expression<'a> },
    Relation { left: &'a Expression<'a>, op: RelationOp, right: &'a Expression<'a> },

    Ternary { condition: &'a Expression<'a>, left: &'a Expression<'a>, right: &'a Expression<'a> },
    Or { left: &'a Expression<'a>, right: &'a Expression<'a> },
    And { left: &'a Expression<'a>, right: &'a Expression<'a> },
    Unary { op: UnaryOp, right: &'a Expression<'a> },

    Member { primary: &'a Expression<'a>, member: &'a Member<'a> },

    Function { identifier: String, arguments: Vec<&'a Expression<'a>> },
    List { members: Vec<&'a Expression<'a>> },
    Map {fields: Vec<(&'a Expression<'a>, &'a Expression<'a>)>},

    Literal(Literal<'a>),
    Ident(String),
}

#[derive(Debug)]
pub enum Member<'a> {
    Attribute { identifier: String },
    Function { identifier: String, arguments: Vec<Expression<'a>> },
    Index { expression: Expression<'a> },
    Fields { fields: Vec<(String, Expression<'a>)> },
}


#[derive(Debug)]
pub enum Literal<'a> {
    Int(i32),
    UInt(u32),
    Float(f64),
    String(&'a str),
    Bool(bool),
    Null,
}
