extern crate core;

use cel_parser::{parse, ExpressionReferences, Member};
use cel_parser::{Expression, Spanned};
use std::convert::TryFrom;
use std::ops::Range;
use std::sync::Arc;
use thiserror::Error;

mod macros;

pub mod context;
pub use cel_parser::error::ParseError;
pub use context::Context;
pub use functions::FunctionContext;
pub use objects::{ResolveResult, Value};
pub mod functions;
mod magic;
pub mod objects;
mod resolvers;

#[cfg(feature = "chrono")]
mod duration;
#[cfg(feature = "chrono")]
pub use ser::{Duration, Timestamp};

mod ser;
pub use ser::to_value;
pub use ser::SerializationError;

#[cfg(feature = "json")]
mod json;
#[cfg(feature = "json")]
pub use json::ConvertToJsonError;

use magic::FromContext;

pub mod extractors {
    pub use crate::magic::{Arguments, Identifier, This};
}

#[derive(Error, Debug, PartialEq, Clone)]
pub enum ExecutionError {
    #[error("Invalid argument count: expected {expected}, got {actual}")]
    InvalidArgumentCount { expected: usize, actual: usize },
    #[error("Invalid argument type: {:?}", .target)]
    UnsupportedTargetType { target: Value },
    #[error("Method '{method}' not supported on type '{target:?}'")]
    NotSupportedAsMethod { method: String, target: Value },
    /// Indicates that the script attempted to use a value as a key in a map,
    /// but the type of the value was not supported as a key.
    #[error("Unable to use value '{0:?}' as a key")]
    UnsupportedKeyType(Value),
    #[error("Unexpected type: got '{got}', want '{want}'")]
    UnexpectedType { got: String, want: String },
    /// Indicates that the script attempted to reference a key on a type that
    /// was missing the requested key.
    #[error("No such key: {0}")]
    NoSuchKey(Arc<Spanned<String>>),
    /// Indicates that the script attempted to reference an undeclared variable
    /// method, or function.
    #[error("Undeclared reference to '{0}'")]
    UndeclaredReference(Arc<Spanned<String>>),
    /// Indicates that a function expected to be called as a method, or to be
    /// called with at least one parameter.
    #[error("Missing argument or target")]
    MissingArgumentOrTarget,
    /// Indicates that a comparison could not be performed.
    #[error("{0:?} can not be compared to {1:?}")]
    ValuesNotComparable(Value, Value),
    /// Indicates that an operator was used on a type that does not support it.
    #[error("Unsupported unary operator '{0}': {1:?}")]
    UnsupportedUnaryOperator(&'static str, Value),
    /// Indicates that an unsupported binary operator was applied on two values
    /// where it's unsupported, for example list + map.
    #[error("Unsupported binary operator '{0}': {1:?}, {2:?}")]
    UnsupportedBinaryOperator(&'static str, Value, Value),
    /// Indicates that an unsupported type was used to index a map
    #[error("Cannot use value as map index: {0:?}")]
    UnsupportedMapIndex(Value),
    /// Indicates that an unsupported type was used to index a list
    #[error("Cannot use value as list index: {0:?}")]
    UnsupportedListIndex(Value),
    /// Indicates that an unsupported type was used to index a list
    #[error("Cannot use value {0:?} to index {1:?}")]
    UnsupportedIndex(Value, Value),
    /// Indicates that a function call occurred without an [`Expression::Ident`]
    /// as the function identifier.
    #[error("Unsupported function call identifier type: {0:?}")]
    UnsupportedFunctionCallIdentifierType(Spanned<Expression>),
    /// Indicates that a [`Member::Fields`] construction was attempted
    /// which is not yet supported.
    #[error("Unsupported fields construction: {0:?}")]
    UnsupportedFieldsConstruction(Member),
    /// Indicates that a function had an error during execution.
    #[error("Error executing function '{function}': {message}")]
    FunctionError { function: String, message: String },
}

impl ExecutionError {
    pub fn no_such_key(name: &str, span: Option<Range<usize>>) -> Self {
        ExecutionError::NoSuchKey(Arc::new(Spanned::new(name.to_string(), span)))
    }

    pub fn undeclared_reference(name: &str, span: Option<Range<usize>>) -> Self {
        ExecutionError::UndeclaredReference(Arc::new(Spanned::new(name.to_string(), span)))
    }

    pub fn invalid_argument_count(expected: usize, actual: usize) -> Self {
        ExecutionError::InvalidArgumentCount { expected, actual }
    }

    pub fn function_error<E: ToString>(function: &str, error: E) -> Self {
        ExecutionError::FunctionError {
            function: function.to_string(),
            message: error.to_string(),
        }
    }

    pub fn unsupported_target_type(target: Value) -> Self {
        ExecutionError::UnsupportedTargetType { target }
    }

    pub fn not_supported_as_method(method: &str, target: Value) -> Self {
        ExecutionError::NotSupportedAsMethod {
            method: method.to_string(),
            target,
        }
    }

    pub fn unsupported_key_type(value: Value) -> Self {
        ExecutionError::UnsupportedKeyType(value)
    }

    pub fn missing_argument_or_target() -> Self {
        ExecutionError::MissingArgumentOrTarget
    }
}

#[derive(Debug)]
pub struct Program {
    expression: Spanned<Expression>,
}

impl Program {
    pub fn compile(source: &str) -> Result<Program, ParseError> {
        parse(source).map(|expression| Program { expression })
    }

    pub fn execute(&self, context: &Context) -> ResolveResult {
        Value::resolve(&self.expression.inner, context)
    }

    /// Returns the variables and functions referenced by the CEL program
    ///
    /// # Example
    /// ```rust
    /// # use cel_interpreter::Program;
    /// let program = Program::compile("size(foo) > 0").unwrap();
    /// let references = program.references();
    ///
    /// assert!(references.has_function("size"));
    /// assert!(references.has_variable("foo"));
    /// ```
    pub fn references(&self) -> ExpressionReferences {
        self.expression.references()
    }
}

impl TryFrom<&str> for Program {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Program::compile(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::objects::{ResolveResult, Value};
    use crate::{ExecutionError, Program};
    use std::collections::HashMap;
    use std::convert::TryInto;

    /// Tests the provided script and returns the result. An optional context can be provided.
    pub(crate) fn test_script(script: &str, ctx: Option<Context>) -> ResolveResult {
        let program = Program::compile(script).unwrap();
        program.execute(&ctx.unwrap_or_default())
    }

    #[test]
    fn parse() {
        Program::compile("1 + 1").unwrap();
    }

    #[test]
    fn from_str() {
        let input = "1.1";
        let _p: Program = input.try_into().unwrap();
    }

    #[test]
    fn variables() {
        fn assert_output(script: &str, expected: ResolveResult) {
            let mut ctx = Context::default();
            ctx.add_variable_from_value("foo", HashMap::from([("bar", 1i64)]));
            ctx.add_variable_from_value("arr", vec![1i64, 2, 3]);
            ctx.add_variable_from_value("str", "foobar".to_string());
            assert_eq!(test_script(script, Some(ctx)), expected);
        }

        // Test methods
        assert_output("size([1, 2, 3]) == 3", Ok(true.into()));
        assert_output("size([]) == 3", Ok(false.into()));

        // Test variable attribute traversals
        assert_output("foo.bar == 1", Ok(true.into()));

        // Test that we can index into an array
        assert_output("arr[0] == 1", Ok(true.into()));

        // Test that we can index into a string
        assert_output("str[0] == 'f'", Ok(true.into()));

        // Test that we can merge two maps
        assert_output(
            "{'a': 1} + {'a': 2, 'b': 3}",
            Ok(HashMap::from([("a", 2), ("b", 3)]).into()),
        );
    }

    #[test]
    fn test_execution_errors() {
        let tests = vec![
            (
                "no such key",
                "foo.baz.bar == 1",
                ExecutionError::no_such_key("baz", Some(4..7)),
            ),
            (
                "undeclared reference",
                "missing == 1",
                ExecutionError::undeclared_reference("missing", Some(0..7)),
            ),
            (
                "undeclared method",
                "1.missing()",
                ExecutionError::undeclared_reference("missing", Some(2..9)),
            ),
            (
                "undeclared function",
                "missing(1)",
                ExecutionError::undeclared_reference("missing", Some(0..7)),
            ),
            (
                "unsupported key type",
                "{null: true}",
                ExecutionError::unsupported_key_type(Value::Null),
            ),
        ];

        for (name, script, error) in tests {
            let mut ctx = Context::default();
            ctx.add_variable_from_value("foo", HashMap::from([("bar", 1)]));
            let res = test_script(script, Some(ctx));
            assert_eq!(res, error.into(), "{}", name);
        }
    }
}
