use std::collections::HashSet;
use std::sync::Arc;

use parse_display::Display;

#[derive(Display, Debug, Eq, PartialEq, Clone)]
pub enum RelationOp {
    #[display("<")]
    LessThan,
    #[display("<=")]
    LessThanEq,
    #[display(">")]
    GreaterThan,
    #[display(">=")]
    GreaterThanEq,
    #[display("==")]
    Equals,
    #[display("!=")]
    NotEquals,
    #[display("in")]
    In,
}

#[derive(Display, Debug, Eq, PartialEq, Clone)]
pub enum ArithmeticOp {
    #[display("+")]
    Add,
    #[display("-")]
    Subtract,
    #[display("*")]
    Divide,
    #[display("/")]
    Multiply,
    #[display("%")]
    Modulus,
}

#[derive(Display, Debug, Eq, PartialEq, Clone)]
pub enum UnaryOp {
    #[display("!")]
    Not,
    #[display("!!")]
    DoubleNot,
    #[display("-")]
    Minus,
    #[display("--")]
    DoubleMinus,
}

#[derive(Display, Debug, PartialEq, Clone)]
pub enum Expression {
    #[display("{0} {1} {2}")]
    Arithmetic(Box<Expression>, ArithmeticOp, Box<Expression>),
    #[display("{0} {1} {2}")]
    Relation(Box<Expression>, RelationOp, Box<Expression>),

    #[display("{0} ? {1} : {2}")]
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    #[display("{0} || {1}")]
    Or(Box<Expression>, Box<Expression>),
    #[display("{0} && {1}")]
    And(Box<Expression>, Box<Expression>),
    #[display("{0} {1}")]
    Unary(UnaryOp, Box<Expression>),

    #[display("{0}{1}")]
    Member(Box<Expression>, Box<Member>),
    #[display("{1}{0}({2})")]
    FunctionCall(
        Box<Expression>,
        #[display(with = display::FunctionCallMember)] Option<Box<Expression>>,
        #[display(with = display::Csv)] Vec<Expression>,
    ),

    #[display("[{0}]")]
    List(#[display(with = display::Csv)] Vec<Expression>),
    #[display("{{{0}}}")]
    Map(#[display(with = display::Map)] Vec<(Expression, Expression)>),

    #[display("{0}")]
    Atom(Atom),
    #[display("{0}")]
    Ident(Arc<String>),
}

mod display;

#[derive(Display, Debug, PartialEq, Clone)]
pub enum Member {
    #[display(".{0}")]
    Attribute(Arc<String>),
    #[display("[{0}]")]
    Index(Box<Expression>),
    #[display("{{{0}}}")]
    Fields(#[display(with = display::Map)] Vec<(Arc<String>, Expression)>),
}

#[derive(Display, Debug, PartialEq, Clone)]
pub enum Atom {
    #[display("{0}")]
    Int(i64),
    #[display("{0}")]
    UInt(u64),
    #[display("{0}")]
    Float(f64),
    #[display("{0}")]
    String(#[display(with = display::StringAtom)] Arc<String>),
    #[display("{0}")]
    Bytes(#[display(with = display::BytesAtom)] Arc<Vec<u8>>),
    #[display("{0}")]
    Bool(bool),
    #[display("null")]
    Null,
}

/// A collection of all the references that an expression makes to variables and functions.
pub struct ExpressionReferences<'expr> {
    variables: HashSet<&'expr str>,
    functions: HashSet<&'expr str>,
}

impl<'expr> ExpressionReferences<'expr> {
    /// Returns true if the expression references the provided variable name.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("foo.bar == true").unwrap();
    /// let references = expression.references();
    /// assert!(references.has_variable("foo"));
    /// ```
    pub fn has_variable(&self, name: impl AsRef<str>) -> bool {
        self.variables.contains(name.as_ref())
    }

    /// Returns true if the expression references the provided variable name.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("size(foo) > 0").unwrap();
    /// let references = expression.references();
    /// assert!(references.has_function("size"));
    /// ```
    pub fn has_function(&self, name: impl AsRef<str>) -> bool {
        self.functions.contains(name.as_ref())
    }

    /// Returns a list of all variables referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("foo.bar == true").unwrap();
    /// let references = expression.references();
    /// assert_eq!(vec!["foo"], references.variables());
    /// ```
    pub fn variables(&self) -> Vec<&str> {
        self.variables.iter().copied().collect()
    }

    /// Returns a list of all functions referenced in the expression.
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("size(foo) > 0").unwrap();
    /// let references = expression.references();
    /// assert_eq!(vec!["size"], references.functions());
    /// ```
    pub fn functions(&self) -> Vec<&str> {
        self.functions.iter().copied().collect()
    }
}

impl Expression {
    /// Returns a set of all variables referenced in the expression. Variable identifiers
    /// are represented internally as [`Arc<String>`] and this function simply clones those
    /// references into a [`HashSet`].
    ///
    /// # Example
    /// ```rust
    /// # use cel_parser::parse;
    /// let expression = parse("foo && size(foo) > 0").unwrap();
    /// let references = expression.references();
    ///
    /// assert!(references.has_variable("foo"));
    /// assert!(references.has_function("size"));
    /// ```
    pub fn references(&self) -> ExpressionReferences {
        let mut variables = HashSet::new();
        let mut functions = HashSet::new();
        self._references(&mut variables, &mut functions);
        ExpressionReferences {
            variables,
            functions,
        }
    }

    /// Internal recursive function to collect all variable and function references in the expression.
    fn _references<'expr>(
        &'expr self,
        variables: &mut HashSet<&'expr str>,
        functions: &mut HashSet<&'expr str>,
    ) {
        match self {
            Expression::Arithmetic(e1, _, e2)
            | Expression::Relation(e1, _, e2)
            | Expression::Ternary(e1, _, e2)
            | Expression::Or(e1, e2)
            | Expression::And(e1, e2) => {
                e1._references(variables, functions);
                e2._references(variables, functions);
            }
            Expression::Unary(_, e) => {
                e._references(variables, functions);
            }
            Expression::Member(e, _) => {
                e._references(variables, functions);
            }
            Expression::FunctionCall(name, target, args) => {
                if let Expression::Ident(v) = &**name {
                    functions.insert(v.as_str());
                }
                if let Some(target) = target {
                    target._references(variables, functions);
                }
                for e in args {
                    e._references(variables, functions);
                }
            }
            Expression::List(e) => {
                for e in e {
                    e._references(variables, functions);
                }
            }
            Expression::Map(v) => {
                for (e1, e2) in v {
                    e1._references(variables, functions);
                    e2._references(variables, functions);
                }
            }
            Expression::Atom(_) => {}
            Expression::Ident(v) => {
                variables.insert(v.as_str());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse;

    #[test]
    fn test_references() {
        let expr =
            parse("foo.bar.baz == true && size(foo) > 0 && foo[0] == 1 && bar.startsWith('a')")
                .unwrap();
        let refs = expr.references();
        assert!(refs.has_variable("foo"));
        assert!(refs.has_variable("bar"));
        assert_eq!(refs.variables.len(), 2);

        assert!(refs.has_function("size"));
        assert!(refs.has_function("startsWith"));
        assert_eq!(refs.functions.len(), 2);
    }

    #[test]
    fn test_display() {
        let expr =
            parse("foo.bar.baz == true && size(foo) > 0 && foo[0] == 1 && bar.startsWith('a')")
                .unwrap();
        assert_eq!(
            &expr.to_string(),
            "foo.bar.baz == true && size(foo) > 0 && foo.[0] == 1 && bar.startsWith(\"a\")"
        );
    }
}
