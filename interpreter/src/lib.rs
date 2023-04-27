use crate::context::Context;
use crate::objects::{CelType, ResolveResult};
use cel_parser::ast::Expression;
use cel_parser::parser::ExpressionParser;
use std::convert::TryFrom;
use std::rc::Rc;
use thiserror::Error;

pub mod context;
mod functions;
pub mod objects;

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
    UnsupportedTargetType { target: CelType },
    #[error("Method '{method}' not supported on type '{target:?}'")]
    NotSupportedAsMethod { method: String, target: CelType },
    /// Indicates that the script attempted to use a value as a key in a map,
    /// but the type of the value was not supported as a key.
    #[error("Unable to use value '{0:?}' as a key")]
    UnsupportedKeyType(CelType),
    /// Indicates that the script attempted to reference a key on a type that
    /// was missing the requested key.
    #[error("No such key: {0}")]
    NoSuchKey(Rc<String>),
    /// Indicates that the script attempted to reference an undeclared variable
    /// method, or function.
    #[error("Undeclared reference to '{0}'")]
    UndeclaredReference(Rc<String>),
    /// Indicates that a function expected to be called as a method, or to be
    /// called with at least one parameter.
    #[error("Missing argument or target")]
    MissingArgumentOrTarget,
}

impl ExecutionError {
    pub fn no_such_key(name: &str) -> Self {
        ExecutionError::NoSuchKey(Rc::new(name.to_string()))
    }

    pub fn undeclared_reference(name: &str) -> Self {
        ExecutionError::UndeclaredReference(Rc::new(name.to_string()))
    }

    pub fn invalid_argument_count(expected: usize, actual: usize) -> Self {
        ExecutionError::InvalidArgumentCount { expected, actual }
    }

    pub fn unsupported_target_type(target: CelType) -> Self {
        ExecutionError::UnsupportedTargetType { target }
    }

    pub fn not_supported_as_method(method: &str, target: CelType) -> Self {
        ExecutionError::NotSupportedAsMethod {
            method: method.to_string(),
            target,
        }
    }

    pub fn unsupported_key_type(value: CelType) -> Self {
        ExecutionError::UnsupportedKeyType(value)
    }
}

#[derive(Debug)]
pub struct Program {
    expression: Expression,
}

impl Program {
    pub fn compile(source: &str) -> Result<Program, ParseError> {
        match ExpressionParser::new().parse(source) {
            Ok(expression) => Ok(Program { expression }),
            // To-Do: Better error handling
            Err(e) => Err(ParseError {
                msg: format!("{}", e),
            }),
        }
    }

    pub fn execute(&self, context: &Context) -> ResolveResult {
        CelType::resolve(&self.expression, context)
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
    use crate::objects::{CelType, ResolveResult};
    use crate::{functions, ExecutionError, Program};
    use std::collections::HashMap;
    use std::convert::TryInto;
    use std::rc::Rc;

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
            ctx.add_variable("foo", HashMap::from([("bar", 1)]));
            ctx.add_variable("arr", vec![1i32, 2, 3]);
            ctx.add_variable("str", "foobar".to_string());
            let program = Program::compile(script).unwrap();
            let res = program.execute(&ctx);
            assert_eq!(res, expected);
        }

        // Test methods
        assert_output("[1, 2, 3].size() == 3", Ok(true.into()));
        assert_output("[].size() == 3", Ok(false.into()));

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
                ExecutionError::no_such_key("baz"),
            ),
            (
                "invalid argument count",
                "has(foo, bar)",
                ExecutionError::invalid_argument_count(1, 2),
            ),
            (
                "undeclared reference",
                "missing == 1",
                ExecutionError::undeclared_reference("missing"),
            ),
            (
                "unsupported key type",
                "{null: true}",
                ExecutionError::unsupported_key_type(CelType::Null),
            ),
        ];

        for (name, script, error) in tests {
            let program = Program::compile(script).unwrap();
            let mut ctx = Context::default();
            ctx.add_variable("foo", HashMap::from([("bar", 1)]));
            let res = program.execute(&ctx);
            assert_eq!(res, error.into(), "{}", name);
        }
    }
}
