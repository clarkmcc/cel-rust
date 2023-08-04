use lalrpop_util::lalrpop_mod;

pub mod ast;
pub use ast::*;

use std::fmt::Display;

lalrpop_mod!(#[allow(clippy::all)] pub parser, "/cel.rs");


#[derive(Debug)]
pub struct ParseError {
    msg: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

pub fn parse(input: &str) -> Result<Expression, ParseError> {
    // Wrap the internal parser function - whether larlpop or chumsky
    println!("Parsing: >>>{}<<<", input);


    // Example for a possible new chumsky based parser...
    // parser().parse(input)
    //     .into_result()
    //     .map_err(|e|  {
    //         ParseError {
    //             msg: e.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("\n")
    //         }
    //     })


    // Existing Larlpop Parser:
    crate::parser::ExpressionParser::new()
        .parse(input)
        .map_err(|e| ParseError {
            msg: format!("{}", e),
        })

}
