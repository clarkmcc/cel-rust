use crate::ast::{CallExpr, ComprehensionExpr, Expr, IdedExpr};
use crate::reference::Val::Boolean;
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

pub fn exists_macro_expander(
    helper: &mut ParserHelper,
    target: Option<IdedExpr>,
    mut args: Vec<IdedExpr>,
) -> IdedExpr {
    let mut arguments = vec![args.remove(1)];
    let v = match args.remove(0).expr {
        Expr::Ident(ident) => ident,
        _ => panic!("Not an ident expression"),
    };

    let init = helper.next_expr(Expr::Literal(Boolean(false)));
    let result_binding = "@result".to_string();
    let accu_ident = helper.next_expr(Expr::Ident(result_binding.clone()));
    let arg = helper.next_expr(Expr::Call(CallExpr {
        func_name: "!_".to_string(),
        target: None,
        args: vec![accu_ident],
    }));
    let condition = helper.next_expr(Expr::Call(CallExpr {
        func_name: "@not_strictly_false".to_string(),
        target: None,
        args: vec![arg],
    }));

    arguments.insert(0, helper.next_expr(Expr::Ident(result_binding.clone())));
    let step = helper.next_expr(Expr::Call(CallExpr {
        func_name: "_||_".to_string(),
        target: None,
        args: arguments,
    }));

    let result = helper.next_expr(Expr::Ident(result_binding.clone()));

    helper.next_expr(Expr::Comprehension(ComprehensionExpr {
        iter_range: Box::new(target.unwrap()),
        iter_var: v,
        iter_var2: None,
        accu_var: result_binding,
        accu_init: init.into(),
        loop_cond: condition.into(),
        loop_step: step.into(),
        result: result.into(),
    }))
}
pub fn all_macro_expander(
    helper: &mut ParserHelper,
    target: Option<IdedExpr>,
    mut args: Vec<IdedExpr>,
) -> IdedExpr {
    let mut arguments = vec![args.remove(1)];
    let v = match args.remove(0).expr {
        Expr::Ident(ident) => ident,
        _ => panic!("Not an ident expression"),
    };

    let init = helper.next_expr(Expr::Literal(Boolean(true)));
    let result_binding = "@result".to_string();
    let accu_ident = helper.next_expr(Expr::Ident(result_binding.clone()));
    let condition = helper.next_expr(Expr::Call(CallExpr {
        func_name: "@not_strictly_false".to_string(),
        target: None,
        args: vec![accu_ident],
    }));

    arguments.insert(0, helper.next_expr(Expr::Ident(result_binding.clone())));
    let step = helper.next_expr(Expr::Call(CallExpr {
        func_name: "_&&_".to_string(),
        target: None,
        args: arguments,
    }));

    let result = helper.next_expr(Expr::Ident(result_binding.clone()));

    helper.next_expr(Expr::Comprehension(ComprehensionExpr {
        iter_range: Box::new(target.unwrap()),
        iter_var: v,
        iter_var2: None,
        accu_var: result_binding,
        accu_init: init.into(),
        loop_cond: condition.into(),
        loop_step: step.into(),
        result: result.into(),
    }))
}
