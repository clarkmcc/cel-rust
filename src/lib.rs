use pest_derive::Parser;
use pest::prec_climber::{PrecClimber, Operator, Assoc};
use lazy_static::lazy_static;

#[derive(Parser)]
#[grammar = "cel.pest"]
pub struct IdentParser;


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
