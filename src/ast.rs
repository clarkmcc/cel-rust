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
pub enum Expression {
    Arithmetic { left: Box<Expression>, op: ArithmeticOp, right: Box<Expression> },
    Relation { left: Box<Expression>, op: RelationOp, right: Box<Expression> },

    Ternary { condition: Box<Expression>, left: Box<Expression>, right: Box<Expression> },
    Or { left: Box<Expression>, right: Box<Expression> },
    And { left: Box<Expression>, right: Box<Expression> },
    Unary { op: UnaryOp, right: Box<Expression> },

    Member { primary: Box<Expression>, member: Member },

    Function { identifier: String, arguments: Vec<Box<Expression>> },
    List { members: Vec<Box<Expression>> },
    Map {fields: Vec<(Box<Expression>, Box<Expression>)>},

    Literal(Literal),
    Ident(String),
}

#[derive(Debug)]
pub enum Member {
    Attribute { identifier: String },
    Function { identifier: String, arguments: Vec<Box<Expression>> },
    Index { expression: Box<Expression> },
    Fields { fields: Vec<(String, Box<Expression>)> },
}


#[derive(Debug)]
pub enum Literal {
    Int(i32)
}