use crate::context::Context;
use crate::objects::{CelKey, CelSet, CelType};
use crate::ExecutionError;
use cel_parser::Expression;
use std::collections::HashSet;
use std::convert::TryInto;
use std::hash::Hash;
use std::rc::Rc;

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
    match (target, args.len()) {
        // We can assume that we will have either gotten a target or an arg, but we should
        // have only gotten one of either, and not more than one arg.
        (Some(target), 0) => target.size().map(|v| CelType::Int(v as i32)),
        (None, 1) => CelType::resolve(&args[0], ctx)?
            .size()
            .map(|v| CelType::Int(v as i32)),
        (Some(_), _) => Err(ExecutionError::invalid_argument_count(0, args.len()))?,
        (None, _) => Err(ExecutionError::invalid_argument_count(1, args.len()))?,
    }
}

/// Returns true if the target contains the provided argument. The actual behavior
/// depends mainly on the type of the target.
///
/// The following [`CelType`] variants are supported:
/// * [`CelType::List`] - Returns true if the list contains the provided value.
/// * [`CelType::Map`] - Returns true if the map contains the provided key.
/// * [`CelType::Set`] - Returns true if the set contains the provided value.
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
/// ## Set
/// ```cel
/// {{1, 2, 3}}.contains(1) == true
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
        CelType::Set(v) => v
            .set
            .contains(&arg.try_into().map_err(ExecutionError::UnsupportedKeyType)?),
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

/// Attempts to convert the target into a [`CelType::Set`].
///
/// The following [`CelType`] variants are supported:
/// * [`CelType::List`] - Returns a set containing the values of the list.
/// * [`CelType::Map`] - Returns a set containing the keys of the map.
/// * [`CelType::Set`] - Returns a cloned copy of the set.
/// * [`CelType::Int`] - Returns a set containing the value of the int.
/// * [`CelType::String`] - Returns a set the string as a single value.
/// * [`CelType::Bool`] - Returns a set containing the value of the bool.
///
// # Example
///
/// ## List
/// ```cel
/// [1, 2, 3].intoSet() == {{1, 2, 3}}
/// ```
///
/// ## Map
/// ```cel
/// {"a": 1, "b": 2, "c": 3}.intoSet() == {{'a', 'b', 'c'}}
/// ```
///
/// ## Set
/// ```cel
/// {{1, 2, 3}}.intoSet() == {{1, 2, 3}}
/// ```
///
/// ## String
/// ```cel
/// "abc".intoSet() == {{"abc"}}
/// ```
///
/// ## Bool
/// ```cel
/// true.intoSet() == {{true}}
/// ```
pub fn into_set(
    target: Option<&CelType>,
    _: &[Expression],
    _: &Context,
) -> Result<CelType, ExecutionError> {
    let target = target.ok_or(ExecutionError::MissingArgumentOrTarget)?;
    let type_to_key = |t: &CelType| -> Result<CelKey, ExecutionError> {
        t.clone()
            .try_into()
            .map_err(ExecutionError::UnsupportedKeyType)
    };
    let key_to_key = |t: &CelKey| -> Result<CelKey, ExecutionError> { Ok(t.clone()) };
    match target {
        CelType::List(v) => iter_to_set(v.iter(), type_to_key),
        CelType::Map(v) => iter_to_set(v.map.keys(), key_to_key),
        CelType::Set(v) => Ok(CelType::Set(v.clone())),
        CelType::Int(v) => Ok(value_to_set(*v)),
        CelType::UInt(v) => Ok(value_to_set(*v)),
        CelType::String(v) => Ok(value_to_set(v.clone())),
        CelType::Bool(v) => Ok(value_to_set(*v)),
        _ => Err(ExecutionError::function_error(
            "into_set",
            &format!("cannot convert type to set: {:?}", target),
        ))?,
    }
}

/// Helper function to convert an iterator over T into a [`CelType::Set`] using
/// the provided mapper function.
pub fn iter_to_set<T, I, F>(iter: I, mapper: F) -> Result<CelType, ExecutionError>
where
    F: Fn(T) -> Result<CelKey, ExecutionError>,
    I: Iterator<Item = T>,
{
    let mut set: HashSet<CelKey> = HashSet::new();
    for item in iter {
        mapper(item).map(|item| set.insert(item))?;
    }
    Ok(CelType::Set(CelSet { set: Rc::new(set) }))
}

/// Converts a single value into a [`CelType::Set`] so long as the
/// value can be converted into a [`CelKey`].
fn value_to_set<T>(value: T) -> CelType
where
    T: Hash + Eq + Into<CelKey>,
{
    let mut set = HashSet::new();
    set.insert(value.into());
    CelType::Set(CelSet { set: Rc::new(set) })
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
