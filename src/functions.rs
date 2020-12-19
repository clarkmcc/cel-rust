use crate::ast::Expression;
use crate::context::Context;
use crate::objects::CelType;

pub fn size<'a>(target: Option<&'a CelType<'a>>, args: &[Expression], context: &Context<'a>) -> CelType<'a> {
    dbg!(target);
    dbg!(args);
    CelType::Int(1)
}
