use crate::context::Context;
use crate::objects::CelType;
use crate::ExecutionError;
use cel_parser::Expression;
use std::convert::TryInto;
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

/// Returns true if the provided argument can be resolved. This function is
/// useful for checking if a property exists on a type before attempting to
/// resolve it. Resolving a property that does not exist will result in a
/// [`ExecutionError::NoSuchKey`] error.
///
/// Operates similar to the `has` macro describe in the Go CEL implementation
/// spec: https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros.
///
/// # Examples
/// ```cel
/// has(foo.bar.baz)
/// ```
pub fn has(
    target: Option<&CelType>,
    args: &[Expression],
    ctx: &Context,
) -> Result<CelType, ExecutionError> {
    if target.is_some() {
        return Err(ExecutionError::not_supported_as_method(
            "has",
            target.cloned().unwrap(),
        ));
    }
    let arg = args
        .get(0)
        .ok_or(ExecutionError::invalid_argument_count(1, 0))?;

    // We determine if a type has a property by attempting to resolve it.
    // If we get a NoSuchKey error, then we know the property does not exist
    match CelType::resolve(arg, ctx) {
        Ok(_) => CelType::Bool(true),
        Err(err) => match err {
            ExecutionError::NoSuchKey(_) => CelType::Bool(false),
            _ => return Err(err),
        },
    }
    .into()
}

/// Maps the provided list to a new list by applying an expression to each
/// input item. This function is intended to be used like the CEL-go `map`
/// macro: https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros
///
/// # Examples
/// ```cel
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
/// ```
pub fn map(
    target: Option<&CelType>,
    args: &[Expression],
    ctx: &Context,
) -> Result<CelType, ExecutionError> {
    macro_wrapper(ctx, target, args, op_map)
}

/// Filters the provided list by applying an expression to each input item
/// and including the input item in the resulting list, only if the expression
/// returned true.
///
/// This function is intended to be used like the CEL-go `filter` macro:
/// https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros
///
/// # Example
/// ```cel
/// [1, 2, 3].filter(x, x > 1) == [2, 3]
/// ```
pub fn filter(
    target: Option<&CelType>,
    args: &[Expression],
    ctx: &Context,
) -> Result<CelType, ExecutionError> {
    macro_wrapper(ctx, target, args, op_filter)
}

/// Returns a boolean value indicating whether every value in the provided
/// list met the predicate defined by the provided expression.
///
/// This function is intended to be used like the CEL-go `all` macro:
/// https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros
///
/// # Example
/// ```cel
/// [1, 2, 3].all(x, x > 0) == true
/// ```
pub fn all(
    target: Option<&CelType>,
    args: &[Expression],
    ctx: &Context,
) -> Result<CelType, ExecutionError> {
    macro_wrapper(ctx, target, args, op_all)
}

#[inline(always)]
fn op_map(
    ctx: &Context,
    target: &CelType,
    ident: Rc<String>,
    expr: &Expression,
) -> Result<CelType, ExecutionError> {
    match target {
        CelType::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            let mut ctx = ctx.clone();
            for item in items.iter() {
                ctx.add_variable(&**ident, item.clone());
                let value = CelType::resolve(expr, &ctx)?;
                values.push(value);
            }
            CelType::List(Rc::new(values))
        }
        _ => list_only("map")?,
    }
    .into()
}

#[inline(always)]
fn op_filter(
    ctx: &Context,
    target: &CelType,
    ident: Rc<String>,
    expr: &Expression,
) -> Result<CelType, ExecutionError> {
    match target {
        CelType::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            let mut ctx = ctx.clone();
            for item in items.iter() {
                ctx.add_variable(&**ident, item.clone());
                if let CelType::Bool(true) = CelType::resolve(expr, &ctx)? {
                    values.push(item.clone());
                }
            }
            CelType::List(Rc::new(values))
        }
        _ => list_only("filter")?,
    }
    .into()
}

#[inline(always)]
fn op_all(
    ctx: &Context,
    target: &CelType,
    ident: Rc<String>,
    expr: &Expression,
) -> Result<CelType, ExecutionError> {
    match target {
        CelType::List(items) => {
            let mut ctx = ctx.clone();
            for item in items.iter() {
                ctx.add_variable(&**ident, item.clone());
                if let CelType::Bool(false) = CelType::resolve(expr, &ctx)? {
                    return Ok(CelType::Bool(false));
                }
            }
            return Ok(CelType::Bool(true));
        }
        _ => list_only("all")?,
    }
    .into()
}

#[inline(always)]
fn macro_wrapper(
    ctx: &Context,
    target: Option<&CelType>,
    args: &[Expression],
    func: fn(&Context, &CelType, Rc<String>, &Expression) -> Result<CelType, ExecutionError>,
) -> Result<CelType, ExecutionError> {
    let target = target.ok_or(ExecutionError::missing_argument_or_target())?;
    let ident = get_ident_arg(0, args)?;
    let expr = get_arg(1, args)?;
    func(ctx, target, ident, expr)
}

#[inline(always)]
fn list_only(name: &str) -> Result<CelType, ExecutionError> {
    Err(ExecutionError::function_error(
        name,
        "can only be called on a list",
    ))
}

#[inline(always)]
fn get_arg(idx: usize, args: &[Expression]) -> Result<&Expression, ExecutionError> {
    args.get(idx)
        .ok_or(ExecutionError::invalid_argument_count(idx + 1, args.len()))
}

#[inline(always)]
fn get_ident_arg(idx: usize, args: &[Expression]) -> Result<Rc<String>, ExecutionError> {
    match args
        .get(idx)
        .ok_or(ExecutionError::invalid_argument_count(idx + 1, args.len()))?
    {
        Expression::Ident(ident) => Ok(ident.clone()),
        _ => Err(ExecutionError::function_error(
            "map",
            &format!("argument {} must be an identifier", idx),
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::testing::test_script;
    use std::collections::HashMap;

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

    #[test]
    fn test_has() {
        let tests = vec![
            ("map has", "has(foo.bar) == true"),
            ("map has", "has(foo.bar) == true"),
            ("map not has", "has(foo.baz) == false"),
            ("map deep not has", "has(foo.baz.bar) == false"),
        ];

        for (name, script) in tests {
            let mut ctx = Context::default();
            ctx.add_variable("foo", HashMap::from([("bar", 1)]));
            assert_eq!(test_script(script, Some(ctx)), Ok(true.into()), "{}", name);
        }
    }

    #[test]
    fn test_map() {
        let tests = vec![
            ("map list", "[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
            ("map list 2", "[1, 2, 3].map(y, y + 1) == [2, 3, 4]"),
            (
                "nested map",
                "[[1, 2], [2, 3]].map(x, x.map(x, x * 2)) == [[2, 4], [4, 6]]",
            ),
        ];

        for (name, script) in tests {
            let ctx = Context::default();
            assert_eq!(test_script(script, Some(ctx)), Ok(true.into()), "{}", name);
        }
    }

    #[test]
    fn test_filter() {
        let tests = vec![("filter list", "[1, 2, 3].filter(x, x > 2) == [3]")];

        for (name, script) in tests {
            let ctx = Context::default();
            assert_eq!(test_script(script, Some(ctx)), Ok(true.into()), "{}", name);
        }
    }

    #[test]
    fn test_all() {
        let tests = vec![
            ("all list #1", "[0, 1, 2].all(x, x >= 0)"),
            ("all list #2", "[0, 1, 2].all(x, x > 0) == false"),
        ];

        for (name, script) in tests {
            let ctx = Context::default();
            assert_eq!(test_script(script, Some(ctx)), Ok(true.into()), "{}", name);
        }
    }
}
