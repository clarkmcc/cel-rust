extern crate core;

use cel_parser::{parse, ExpressionReferences};
use std::convert::TryFrom;
use std::sync::Arc;
use thiserror::Error;

mod macros;

pub mod context;
pub use cel_parser::Expression;
pub use context::Context;
pub use functions::FunctionContext;
pub use objects::{ResolveResult, Value};
mod duration;
pub mod functions;
mod magic;
pub mod objects;
mod resolvers;
mod ser;
pub use ser::to_value;
#[cfg(test)]
mod testing;
use magic::FromContext;

pub mod extractors {
    pub use crate::magic::{Arguments, Identifier, This};
}

#[derive(Error, Debug)]
#[error("Error parsing {msg}")]
pub struct ParseError {
    msg: String,
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
    NoSuchKey(Arc<String>),
    /// Indicates that the script attempted to reference an undeclared variable
    /// method, or function.
    #[error("Undeclared reference to '{0}'")]
    UndeclaredReference(Arc<String>),
    /// Indicates that a function expected to be called as a method, or to be
    /// called with at least one parameter.
    #[error("Missing argument or target")]
    MissingArgumentOrTarget,
    /// Indicates that a comparison could not be performed.
    #[error("{0:?} can not be compared to {1:?}")]
    ValuesNotComparable(Value, Value),
    /// Indicates that a function had an error during execution.
    #[error("Error executing function '{function}': {message}")]
    FunctionError { function: String, message: String },
}

impl ExecutionError {
    pub fn no_such_key(name: &str) -> Self {
        ExecutionError::NoSuchKey(Arc::new(name.to_string()))
    }

    pub fn undeclared_reference(name: &str) -> Self {
        ExecutionError::UndeclaredReference(Arc::new(name.to_string()))
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
    expression: Expression,
}

impl Program {
    pub fn compile(source: &str) -> Result<Program, ParseError> {
        match parse(source) {
            Ok(expression) => Ok(Program { expression }),
            // To-Do: Better error handling
            Err(e) => Err(ParseError {
                msg: format!("{}", e),
            }),
        }
    }

    pub fn execute(&self, context: &Context) -> ResolveResult {
        Value::resolve(&self.expression, context)
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
    use crate::testing::test_script;
    use crate::ExecutionError::FunctionError;
    use crate::{ExecutionError, Program};
    use std::collections::HashMap;
    use std::convert::TryInto;

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
    fn test_contains() {
        let tests = vec![
            ("list", "[1, 2, 3].contains(3) == true"),
            ("map", "{1: true, 2: true, 3: true}.contains(3) == true"),
            ("string", "'foobar'.contains('bar') == true"),
            ("bytes", "b'foobar'.contains(b'o') == true"),
        ];

        for (name, script) in tests {
            assert_eq!(test_script(script, None), Ok(true.into()), "{}", name);
        }
    }

    #[test]
    fn test_matches() {
        let tests = vec![
            ("string", "'foobar'.matches('^[a-zA-Z]*$') == true"),
            (
                "map",
                "{'1': 'abc', '2': 'def', '3': 'ghi'}.all(key, key.matches('^[a-zA-Z]*$')) == false",
            ),
        ];

        for (name, script) in tests {
            assert_eq!(
                test_script(script, None),
                Ok(true.into()),
                ".matches failed for '{name}'"
            );
        }
    }

    #[test]
    fn test_matches_err() {
        assert_eq!(
            test_script(
                "'foobar'.matches('(foo') == true", None),
            Err(
                FunctionError {
                    function: "matches".to_string(),
                    message: "'(foo' not a valid regex:\nregex parse error:\n    (foo\n    ^\nerror: unclosed group".to_string()
                }
            )
        );
    }

    #[test]
    fn test_execution_errors() {
        let tests = vec![
            (
                "no such key",
                "foo.baz.bar == 1",
                ExecutionError::no_such_key("baz"),
            ),
            (
                "undeclared reference",
                "missing == 1",
                ExecutionError::undeclared_reference("missing"),
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
