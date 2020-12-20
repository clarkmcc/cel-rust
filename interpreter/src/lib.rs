use crate::context::Context;
use crate::objects::CelType;
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

    pub fn execute(&self, context: &Context) -> CelType {
        CelType::resolve(&self.expression, &context)
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
    use crate::Program;
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
}
