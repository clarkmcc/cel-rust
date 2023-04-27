use crate::context::Context;
use crate::objects::CelType;
use crate::ExecutionError;
use cel_parser::Expression;

/// Calculates the size of either the target, or the provided args depending on how
/// the function is called. If called as a method, the target will be used. If called
/// as a function, the first argument will be used.
///
/// The following [`CelType`] variants are supported:
/// * [`CelType::List`]
/// * [`CelType::Map`]
/// * [`CelType::String`]
/// * [`CelType::Bytes`]
///
/// If `size` is called on an unsupported type, an [`ExecutionError::UnsupportedTargetType`]
/// error will be returned.
///
/// # Examples
///
/// ## Method Form
/// ```skip
/// [1, 2, 3].size() == 3
/// ```
///
/// ## Function Form
/// ```skip
/// size([1, 2, 3]) == 3
/// ```
pub fn size(
    target: Option<&CelType>,
    args: &[Expression],
    ctx: &Context,
) -> Result<CelType, ExecutionError> {
    CelType::Int(match (target, args.len()) {
        // We can assume that we will have either gotten a target or an arg, but we should
        // have only gotten one of either, and not more than one arg.
        (Some(target), 0) => target.size()?,
        (None, 1) => CelType::resolve(&args[0], ctx)?.size()?,
        (Some(_), _) => Err(ExecutionError::invalid_argument_count(0, args.len()))?,
        (None, _) => Err(ExecutionError::invalid_argument_count(1, args.len()))?,
    } as i32)
    .into()
}

pub fn has(
    target: Option<&CelType>,
    args: &[Expression],
    _: &Context,
) -> Result<CelType, ExecutionError> {
    if let Some(target) = target {
        return ExecutionError::not_supported_as_method("has".into(), target.clone()).into();
    } else if args.len() != 1 {
        return ExecutionError::invalid_argument_count(1, args.len()).into();
    }
    Ok(CelType::Bool(true))
}
