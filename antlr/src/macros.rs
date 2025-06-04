use crate::ast::{Expr, IdedExpr};
use crate::ParserHelper;

pub fn has_macro_expander(
    helper: &mut ParserHelper,
    target: Option<IdedExpr>,
    mut args: Vec<IdedExpr>,
) -> IdedExpr {
    if target.is_some() {
        panic!("Got a target when expecting None!")
    }
    if args.len() != 1 {
        panic!("Expected a single arg!")
    }

    match args.remove(0).expr {
        Expr::Select(mut select) => {
            select.test = true;
            helper.next_expr(Expr::Select(select))
        }
        _ => panic!("Not a select expression"),
    }
}
