use crate::context::Context;
use crate::duration::parse_duration;
use crate::objects::CelType;
use crate::{ExecutionError, ResolveResult};
use cel_parser::Expression;
use chrono::{DateTime, Duration, FixedOffset};
use std::convert::TryInto;
use std::mem;
use std::rc::Rc;

/// FunctionCtx is a context object passed to functions when they are called.
/// It contains references ot the target object (if the function is called as
/// a method), the program context ([`Context`]) which gives functions access
/// to variables, and the arguments to the function call.
pub struct FunctionCtx<'t, 'c, 'e> {
    pub name: Rc<String>,
    pub target: Option<&'t CelType>,
    pub ptx: &'c Context<'c>,
    pub args: &'e [Expression],
}

impl<'t, 'c, 'e> FunctionCtx<'t, 'c, 'e> {
    /// Returns a reference to the target object if the function is being called
    /// as a method, or it returns an error if the function is not being called
    /// as a method.
    pub fn target(&self) -> Result<&'t CelType, ExecutionError> {
        self.target
            .ok_or(ExecutionError::missing_argument_or_target())
    }

    /// Checks that the function is not being called as a method, and returns
    /// an error if it is.
    pub fn check_no_method(&self) -> Result<(), ExecutionError> {
        if self.target.is_some() {
            return Err(ExecutionError::not_supported_as_method(
                self.name.as_str(),
                self.target.cloned().unwrap(),
            ));
        }
        Ok(())
    }

    /// Resolves the given expression using the program's [`Context`].
    pub fn resolve(&self, expr: &'e Expression) -> Result<CelType, ExecutionError> {
        self.ptx.resolve(expr)
    }

    /// Resolves all of the given expressions using the program's [`Context`].
    /// The resolved values are returned as a [`CelType::List`].
    pub fn resolve_all(&self, exprs: &[Expression]) -> ResolveResult {
        self.ptx.resolve_all(exprs)
    }

    /// Resolves the argument at the given index. An error is returned if the
    /// argument does not exist.
    pub fn resolve_arg(&self, index: usize) -> Result<CelType, ExecutionError> {
        self.ptx.resolve(self.arg(index)?)
    }

    /// Returns the argument at the given index. An error is returned if the
    /// argument does not exist.
    pub fn arg(&self, index: usize) -> Result<&'e Expression, ExecutionError> {
        self.args
            .get(index)
            .ok_or(ExecutionError::invalid_argument_count(
                index + 1,
                self.args.len(),
            ))
    }

    /// Returns the argument at the given index as a identifier. An error
    /// is returned if the argument does not exist, or if it is not an
    /// identifier.
    pub fn arg_ident(&self, index: usize) -> Result<Rc<String>, ExecutionError> {
        let arg = self.arg(index)?;
        match arg {
            Expression::Ident(ident) => Ok(ident.clone()),
            _ => Err(ExecutionError::function_error(
                "map",
                &format!("argument {} must be an identifier", index),
            )),
        }
    }

    /// Returns an execution error for the currently execution function.
    pub fn error(&self, message: &str) -> Result<CelType, ExecutionError> {
        Err(ExecutionError::function_error(self.name.as_str(), message))
    }
}

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
pub fn size(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    ftx.check_no_method()?;
    let arg = ftx.arg(0)?;
    let size = match ftx.resolve(arg)? {
        CelType::List(l) => l.len(),
        CelType::Map(m) => m.map.len(),
        CelType::String(s) => s.len(),
        CelType::Bytes(b) => b.len(),
        value => return ftx.error(&format!("cannot determine size of {:?}", value)),
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
pub fn contains(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    let target = ftx.target()?;
    let arg = ftx.resolve_arg(0)?;
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

// Performs a type conversion on the target. The following conversions are currently
// supported:
// * `string` - Returns a copy of the target string.
// * `timestamp` - Returns the timestamp in RFC3339 format.
//
// todo: In order to be fully compatible with the CEL specification, this function should
// also support the following conversions:
// * `int`
// * `uint`
// * `double`
// * `bytes`
// * `duration`
pub fn string(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    Ok(match ftx.target()? {
        CelType::String(v) => CelType::String(v.clone()),
        CelType::Timestamp(t) => CelType::String(t.to_rfc3339().into()),
        v => Err(ExecutionError::function_error(
            "string",
            &format!("cannot convert {:?} to string", v),
        ))?,
    })
}

/// Returns true if a string starts with another string.
///
/// # Example
/// ```cel
/// "abc".startsWith("a") == true
/// ```
pub fn starts_with(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    let target = ftx.target()?;
    Ok(match target {
        CelType::String(s) => {
            if let CelType::String(arg) = ftx.resolve_arg(0)? {
                s.starts_with(arg.as_str())
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
pub fn has(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    ftx.check_no_method()?;
    // We determine if a type has a property by attempting to resolve it.
    // If we get a NoSuchKey error, then we know the property does not exist
    match ftx.resolve_arg(0) {
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
pub fn map(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    let ident = ftx.arg_ident(0)?;
    let expr = ftx.arg(1)?;
    match ftx.target()? {
        CelType::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            let mut ptx = ftx.ptx.clone();
            for item in items.iter() {
                ptx.add_variable(&**ident, item.clone());
                let value = ptx.resolve(expr)?;
                values.push(value);
            }
            CelType::List(Rc::new(values))
        }
        _ => ftx.error("only lists are supported")?,
    }
    .into()
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
pub fn filter(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    let ident = ftx.arg_ident(0)?;
    let expr = ftx.arg(1)?;
    match ftx.target()? {
        CelType::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            let mut ptx = ftx.ptx.clone();
            for item in items.iter() {
                ptx.add_variable(&**ident, item.clone());
                if let CelType::Bool(true) = ptx.resolve(expr)? {
                    values.push(item.clone());
                }
            }
            CelType::List(Rc::new(values))
        }
        _ => ftx.error("only lists are supported")?,
    }
    .into()
}

/// Returns a boolean value indicating whether every value in the provided
/// list or map met the predicate defined by the provided expression. If
/// called on a map, the predicate is applied to the map keys.
///
/// This function is intended to be used like the CEL-go `all` macro:
/// https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros
///
/// # Example
/// ```cel
/// [1, 2, 3].all(x, x > 0) == true
/// [{1:true, 2:true, 3:false}].all(x, x > 0) == true
/// ```
pub fn all(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    let ident = ftx.arg_ident(0)?;
    let expr = ftx.arg(1)?;
    match ftx.target()? {
        CelType::List(items) => {
            let mut ptx = ftx.ptx.clone();
            for item in items.iter() {
                ptx.add_variable(&**ident, item.clone());
                if let CelType::Bool(false) = ptx.resolve(expr)? {
                    return Ok(CelType::Bool(false));
                }
            }
            return Ok(CelType::Bool(true));
        }
        CelType::Map(value) => {
            let mut ptx = ftx.ptx.clone();
            for key in value.map.keys() {
                ptx.add_variable(&**ident, key.clone());
                if let CelType::Bool(false) = ptx.resolve(expr)? {
                    return Ok(CelType::Bool(false));
                }
            }
            return Ok(CelType::Bool(true));
        }
        _ => ftx.error("only lists are supported")?,
    }
    .into()
}

/// Duration parses the provided argument into a [`CelType::Duration`] value.
/// The argument must be string, and must be in the format of a duration. See
/// the [`parse_duration`] documentation for more information on the supported
/// formats.
///
/// # Examples
/// - `1h` parses as 1 hour
/// - `1.5h` parses as 1 hour and 30 minutes
/// - `1h30m` parses as 1 hour and 30 minutes
/// - `1h30m1s` parses as 1 hour, 30 minutes, and 1 second
/// - `1ms` parses as 1 millisecond
/// - `1.5ms` parses as 1 millisecond and 500 microseconds
/// - `1ns` parses as 1 nanosecond
/// - `1.5ns` parses as 1 nanosecond (sub-nanosecond durations not supported)
pub fn duration(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    ftx.check_no_method()?;
    let value = ftx.resolve_arg(0)?;
    match value {
        CelType::String(v) => CelType::Duration(_duration(v.as_str())?),
        _ => return Err(ExecutionError::unsupported_target_type(value)),
    }
    .into()
}

pub fn timestamp(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    ftx.check_no_method()?;
    let value = ftx.resolve(ftx.arg(0)?)?;
    match value {
        CelType::String(v) => CelType::Timestamp(_timestamp(v.as_str())?),
        _ => return Err(ExecutionError::unsupported_target_type(value)),
    }
    .into()
}

pub fn max(ftx: FunctionCtx) -> Result<CelType, ExecutionError> {
    match ftx.resolve_all(ftx.args)? {
        CelType::List(items) => {
            if items.is_empty() {
                return Err(ExecutionError::function_error("max", "missing arguments"));
            }
            let items = items.iter().filter(is_numeric).collect::<Vec<_>>();
            let same_type = items
                .iter()
                .all(|item| mem::discriminant(*item) == mem::discriminant(items[0]));
            if !same_type {
                return Err(ExecutionError::function_error(
                    "max",
                    "mixed types not supported",
                ));
            }
            items
                .iter()
                .max_by(|a, b| a.cmp(b))
                .cloned()
                .cloned()
                .unwrap_or(CelType::Null)
                .into()
        }
        _ => ftx.error("only lists are supported"),
    }
}

/// A wrapper around [`parse_duration`] that converts errors into [`ExecutionError`].
/// and only returns the duration, rather than returning the remaining input.
fn _duration(i: &str) -> Result<Duration, ExecutionError> {
    let (_, duration) = parse_duration(i)
        .map_err(|e| ExecutionError::function_error("duration", &e.to_string()))?;
    Ok(duration)
}

fn _timestamp(i: &str) -> Result<DateTime<FixedOffset>, ExecutionError> {
    DateTime::parse_from_rfc3339(i)
        .map_err(|e| ExecutionError::function_error("timestamp", &e.to_string()))
}

fn is_numeric(target: &&CelType) -> bool {
    matches!(
        target,
        CelType::Int(_) | CelType::UInt(_) | CelType::Float(_)
    )
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::testing::test_script;
    use std::collections::HashMap;

    fn assert_script(input: &(&str, &str)) {
        assert_eq!(test_script(input.1, None), Ok(true.into()), "{}", input.0);
    }

    #[test]
    fn test_size() {
        vec![
            ("size of list", "size([1, 2, 3]) == 3"),
            ("size of map", "size({'a': 1, 'b': 2, 'c': 3}) == 3"),
            ("size of string", "size('foo') == 3"),
            ("size of bytes", "size(b'foo') == 3"),
        ]
        .iter()
        .for_each(assert_script);
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
        vec![
            ("map list", "[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
            ("map list 2", "[1, 2, 3].map(y, y + 1) == [2, 3, 4]"),
            (
                "nested map",
                "[[1, 2], [2, 3]].map(x, x.map(x, x * 2)) == [[2, 4], [4, 6]]",
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_filter() {
        vec![("filter list", "[1, 2, 3].filter(x, x > 2) == [3]")]
            .iter()
            .for_each(assert_script);
    }

    #[test]
    fn test_all() {
        vec![
            ("all list #1", "[0, 1, 2].all(x, x >= 0)"),
            ("all list #2", "[0, 1, 2].all(x, x > 0) == false"),
            ("all map", "{0: 0, 1:1, 2:2}.all(x, x >= 0) == true"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_max() {
        vec![
            ("max single", "max(1) == 1"),
            ("max multiple", "max(1, 2, 3) == 3"),
            ("max negative", "max(-1, 0) == 0"),
            ("max float", "max(-1.0, 0.0) == 0.0"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_duration() {
        vec![
            ("duration equal 1", "duration('1s') == duration('1000ms')"),
            ("duration equal 2", "duration('1m') == duration('60s')"),
            ("duration equal 3", "duration('1h') == duration('60m')"),
            ("duration comparison 1", "duration('1m') > duration('1s')"),
            ("duration comparison 2", "duration('1m') < duration('1h')"),
            (
                "duration subtraction",
                "duration('1h') - duration('1m') == duration('59m')",
            ),
            (
                "duration addition",
                "duration('1h') + duration('1m') == duration('1h1m')",
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_starts_with() {
        vec![
            ("starts with true", "'foobar'.startsWith('foo') == true"),
            ("starts with false", "'foobar'.startsWith('bar') == false"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_timestamp() {
        vec![
            (
                "comparison",
                "timestamp('2023-05-29T00:00:00Z') > timestamp('2023-05-28T00:00:00Z')",
            ),
            (
                "comparison",
                "timestamp('2023-05-29T00:00:00Z') < timestamp('2023-05-30T00:00:00Z')",
            ),
            (
                "subtracting duration",
                "timestamp('2023-05-29T00:00:00Z') - duration('24h') == timestamp('2023-05-28T00:00:00Z')",
            ),
            (
                "subtracting date",
                "timestamp('2023-05-29T00:00:00Z') - timestamp('2023-05-28T00:00:00Z') == duration('24h')",
            ),
            (
                "adding duration",
                "timestamp('2023-05-28T00:00:00Z') + duration('24h') == timestamp('2023-05-29T00:00:00Z')",
            ),
            (
                "timestamp string",
                "timestamp('2023-05-28T00:00:00Z').string() == '2023-05-28T00:00:00+00:00'",
            ),
        ]
        .iter()
        .for_each(assert_script);
    }
}
