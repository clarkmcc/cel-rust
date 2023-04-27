use crate::context::Context;
use crate::objects::CelType;
use cel_parser::Expression;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Clone)]
pub enum Error {
    #[error("Invalid argument count: expected {expected}, got {actual}")]
    InvalidArgumentCount { expected: usize, actual: usize },
    #[error("Invalid argument type: {:?}", .target)]
    UnsupportedTargetType { target: CelType },
}

pub type Result = std::result::Result<CelType, Error>;

pub fn size(target: Option<&CelType>, args: &[Expression], _: &Context) -> Result {
    let target = target.unwrap();
    let result = match target {
        CelType::List(l) => l.len(),
        CelType::Map(m) => m.map.len(),
        CelType::String(s) => s.len(),
        CelType::Bytes(b) => b.len(),
        _ => Err(Error::UnsupportedTargetType {
            target: target.clone(),
        })?,
    };
    Ok(CelType::Int(result as i32))
}

pub fn has(target: Option<&CelType>, args: &[Expression], context: &Context) -> Result {
    if args.len() != 1 {
        return Err(Error::InvalidArgumentCount {
            expected: 1,
            actual: args.len(),
        });
    }
    let target = target.unwrap();
    // let result = match target {
    //     CelType::List(l) => l.len() > 0,
    //     CelType::Map(m) => m.map.get(),
    //     CelType::String(s) => s.len() > 0,
    //     CelType::Bytes(b) => b.len() > 0,
    //     _ => unreachable!(),
    // };
    Ok(CelType::Bool(true))
}
