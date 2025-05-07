use std::collections::HashSet;
use std::fmt::Display;
use std::ops::{Deref, DerefMut, Range};
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

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub inner: T,
    #[cfg(feature = "preserve_spans")]
    pub span: Option<Range<usize>>,
}

impl<T> Spanned<T> {
    pub fn new(inner: T, span: Option<Range<usize>>) -> Self {
        Self { inner, span }
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

pub trait SpanExtension: Sized {
    /// Create a new [Spanned<T>] without any span information
    /// e.g. for synthetic variables that do not originate from
    /// actual source code.
    fn unspanned(self) -> Spanned<Self> {
        Spanned {
            inner: self,
            #[cfg(feature = "preserve_spans")]
            span: None,
        }
    }

    #[allow(unused)]
    /// Create a new [Spanned<T>] with a given span. If the
    /// `preserve_spans` feature is turned off, the span is
    ///  just dropped
    fn spanned(self, span: Range<usize>) -> Spanned<Self> {
        Spanned {
            inner: self,
            #[cfg(feature = "preserve_spans")]
            span: Some(span),
        }
    }
}

impl SpanExtension for Expression {}
impl SpanExtension for ArithmeticOp {}
impl SpanExtension for UnaryOp {}
impl SpanExtension for RelationOp {}
impl SpanExtension for String {}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Arithmetic(
        Box<Spanned<Expression>>,
        Spanned<ArithmeticOp>,
        Box<Spanned<Expression>>,
    ),
    Relation(
        Box<Spanned<Expression>>,
        Spanned<RelationOp>,
        Box<Spanned<Expression>>,
    ),

    Ternary(
        Box<Spanned<Expression>>,
        Box<Spanned<Expression>>,
        Box<Spanned<Expression>>,
    ),
    Or(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    And(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expression>>),

    Member(Box<Spanned<Expression>>, Box<Member>),
    FunctionCall(
        Box<Spanned<Expression>>,
        Option<Box<Spanned<Expression>>>,
        Vec<Spanned<Expression>>,
    ),

    List(Vec<Spanned<Expression>>),
    Map(Vec<(Spanned<Expression>, Spanned<Expression>)>),

    Atom(Atom),
    Ident(Arc<Spanned<String>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Member {
    Attribute(Arc<Spanned<String>>),
    Index(Box<Spanned<Expression>>),
    Fields(Vec<(Arc<Spanned<String>>, Spanned<Expression>)>),
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

/// A collection of all the references that an expression makes to variables and functions.
pub struct ExpressionReferences<'expr> {
    variables: HashSet<&'expr str>,
    functions: HashSet<&'expr str>,
}

impl ExpressionReferences<'_> {
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

impl Spanned<Expression> {
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
        match &self.inner {
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
                if let Expression::Ident(v) = &name.inner {
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
}
