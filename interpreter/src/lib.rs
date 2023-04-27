use crate::context::Context;
use crate::objects::{CelType, ResolveResult};
use cel_parser::ast::Expression;
use cel_parser::parser::ExpressionParser;
use std::convert::TryFrom;
use thiserror::Error;

pub mod context;
mod functions;
pub mod objects;

#[derive(Error, Debug)]
#[error("Error parsing {msg}")]
pub struct ParseError {
    msg: String,
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
    use crate::objects::{ResolveError, ResolveResult};
    use crate::{functions, Program};
    use std::collections::HashMap;
    use std::convert::TryInto;
    use std::rc::Rc;

    fn invalid_argument_count(expected: usize, actual: usize) -> ResolveResult {
        Err(ResolveError::FunctionError {
            name: Rc::new("has".into()),
            error: functions::Error::InvalidArgumentCount {
                expected: 1,
                actual: 2,
            },
        })
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
            ctx.add_variable("foo", HashMap::from([("bar", 1)]));
            let program = Program::compile(script).unwrap();
            let res = program.execute(&ctx);
            assert_eq!(res, expected);
        }

        // Test methods
        assert_output("[1, 2, 3].size() == 3", Ok(true.into()));
        assert_output("[].size() == 3", Ok(false.into()));

        // Test variable attribute traversals
        assert_output("foo.bar == 1", Ok(true.into()));

        // Test missing variables
        assert_output("foo.baz == 1", Ok(false.into()));

        // Test that calling the has function with more than one arg returns an error
        assert_output("foo.has(foo, foo)", invalid_argument_count(1, 2));
    }
}
