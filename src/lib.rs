use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub cel); // synthesized by LALRPOP

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
