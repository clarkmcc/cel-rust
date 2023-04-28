use crate::context::Context;
use crate::objects::CelType;
use crate::ExecutionError;
use cel_parser::Expression;
use std::convert::TryInto;

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
/// # Examples
/// ```skip
/// size([1, 2, 3]) == 3
/// ```
pub fn size(
    target: Option<&CelType>,
    args: &[Expression],
    ctx: &Context,
) -> Result<CelType, ExecutionError> {
    if target.is_some() {
        return Err(ExecutionError::not_supported_as_method(
            "size",
            target.cloned().unwrap(),
        ));
    }
    let arg = args
        .get(0)
        .ok_or(ExecutionError::invalid_argument_count(1, 0))?;
    let value = CelType::resolve(arg, ctx)?;
    let size = match value {
        CelType::List(l) => l.len(),
        CelType::Map(m) => m.map.len(),
        CelType::String(s) => s.len(),
        CelType::Bytes(b) => b.len(),
        _ => Err(ExecutionError::function_error(
            "size",
            &format!("cannot determine size of {:?}", value),
        ))?,
    };
    CelType::Int(size as i32).into()
}

/// Returns true if the target contains the provided argument. The actual behavior
/// depends mainly on the type of the target.
///
/// The following [`CelType`] variants are supported:
/// * [`CelType::List`] - Returns true if the list contains the provided value.
/// * [`CelType::Map`] - Returns true if the map contains the provided key.
/// * [`CelType::String`] - Returns true if the string contains the provided substring.
/// * [`CelType::Bytes`] - Returns true if the bytes contain the provided byte.
///
/// # Example
///
/// ## List
/// ```cel
/// [1, 2, 3].contains(1) == true
/// ```
///
/// ## Map
/// ```cel
/// {"a": 1, "b": 2, "c": 3}.contains("a") == true
/// ```
///
/// ## String
/// ```cel
/// "abc".contains("b") == true
/// ```
///
/// ## Bytes
/// ```cel
/// b"abc".contains(b"c") == true
/// ```
pub fn contains(
    target: Option<&CelType>,
    args: &[Expression],
    ctx: &Context,
) -> Result<CelType, ExecutionError> {
    let target = target.unwrap();
    let arg = args
        .get(0)
        .ok_or(ExecutionError::invalid_argument_count(1, 0))?;
    let arg = CelType::resolve(arg, ctx)?;
    Ok(match target {
        CelType::List(v) => v.contains(&arg),
        CelType::Map(v) => v
            .map
            .contains_key(&arg.try_into().map_err(ExecutionError::UnsupportedKeyType)?),
        CelType::String(s) => {
            if let CelType::String(arg) = arg {
                s.contains(arg.as_str())
            } else {
                false
            }
        }
        CelType::Bytes(b) => {
            if let CelType::Bytes(arg) = arg {
                // When search raw bytes, we can only search for a single byte right now.
                let length = arg.len();
                if length > 1 {
                    return Err(ExecutionError::function_error(
                        "contains",
                        &format!("expected 1 byte, found {}", length),
                    ))?;
                }
                arg.as_slice()
                    .first()
                    .map(|byte| b.contains(byte))
                    .unwrap_or(false)
            } else {
                false
            }
        }
        _ => false,
    }
    .into())
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

#[cfg(test)]
mod tests {
    use crate::testing::test_script;

    #[test]
    fn test_size() {
        let tests = vec![
            ("size of list", "size([1, 2, 3]) == 3"),
            ("size of map", "size({'a': 1, 'b': 2, 'c': 3}) == 3"),
            ("size of string", "size('foo') == 3"),
            ("size of bytes", "size(b'foo') == 3"),
        ];

        for (name, script) in tests {
            assert_eq!(test_script(script, None), Ok(true.into()), "{}", name);
        }
    }
}
