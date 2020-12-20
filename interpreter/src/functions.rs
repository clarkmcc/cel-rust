use crate::context::Context;
use crate::objects::CelType;
use cel_parser::Expression;

pub fn size(target: Option<&CelType>, args: &[Expression], context: &Context) -> CelType {
    let target = target.unwrap();
    let result = match target {
        CelType::List(l) => l.len(),
        CelType::Map(m) => m.map.len(),
        CelType::String(s) => s.len(),
        CelType::Bytes(b) => b.len(),
        _ => unreachable!(),
    };
    CelType::Int(result as i32)
}
