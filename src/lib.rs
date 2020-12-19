use crate::ast::{parser::ExpressionParser, Expression};
use crate::objects::CelType;
use std::convert::TryFrom;
use thiserror::Error;

pub mod ast;
pub mod objects;

#[derive(Error, Debug)]
#[error("Error parsing {msg}")]
pub struct ParseError {
    msg: String,
}

#[derive(Debug)]
pub struct Program<'a> {
    expression: Expression<'a>,
}

impl<'a> Program<'a> {
    pub fn compile(source: &'a str) -> Result<Program, ParseError> {
        match ExpressionParser::new().parse(source) {
            Ok(expression) => Ok(Program { expression }),
            // To-Do: Better error handling
            Err(e) => Err(ParseError {
                msg: format!("{}", e),
            }),
        }
    }

    pub fn execute(&self) -> CelType {
        CelType::resolve(&self.expression)
    }
}

impl<'a> TryFrom<&'a str> for Program<'a> {
    type Error = ParseError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
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
