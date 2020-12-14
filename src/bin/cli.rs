use pest::Parser;
use cel_rust::{IdentParser, Rule};

fn main() {
    let pairs = IdentParser::parse(Rule::multiplication, "1 * 1").unwrap_or_else(|e| panic!("{}", e));
    println!("{:?}", pairs);
}