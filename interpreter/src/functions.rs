use crate::context::Context;
use crate::magic::{Arguments, Identifier, This};
use crate::objects::{Value, ValueType};
use crate::resolvers::{Argument, Resolver};
use crate::ExecutionError;
use cel_parser::{Expression, Spanned};
use std::cmp::Ordering;
use std::convert::TryInto;
use std::sync::Arc;

type Result<T> = std::result::Result<T, ExecutionError>;

/// `FunctionContext` is a context object passed to functions when they are called.
///
/// It contains references to the target object (if the function is called as
/// a method), the program context ([`Context`]) which gives functions access
/// to variables, and the arguments to the function call.
#[derive(Clone)]
pub struct FunctionContext<'context> {
    pub name: Arc<Spanned<String>>,
    pub this: Option<Value>,
    pub ptx: &'context Context<'context>,
    pub args: Vec<Spanned<Expression>>,
    pub arg_idx: usize,
}

impl<'context> FunctionContext<'context> {
    pub fn new(
        name: Arc<Spanned<String>>,
        this: Option<Value>,
        ptx: &'context Context<'context>,
        args: Vec<Spanned<Expression>>,
    ) -> Self {
        Self {
            name,
            this,
            ptx,
            args,
            arg_idx: 0,
        }
    }

    /// Resolves the given expression using the program's [`Context`].
    pub fn resolve<R>(&self, resolver: R) -> Result<Value>
    where
        R: Resolver,
    {
        resolver.resolve(self)
    }

    /// Returns an execution error for the currently execution function.
    pub fn error<M: ToString>(&self, message: M) -> ExecutionError {
        ExecutionError::function_error(self.name.as_str(), message)
    }
}

/// Calculates the size of either the target, or the provided args depending on how
/// the function is called.
///
/// If called as a method, the target will be used. If called as a function, the
/// first argument will be used.
///
/// The following [`Value`] variants are supported:
/// * [`Value::List`]
/// * [`Value::Map`]
/// * [`Value::String`]
/// * [`Value::Bytes`]
///
/// # Examples
/// ```skip
/// size([1, 2, 3]) == 3
/// ```
/// ```skip
/// 'foobar'.size() == 6
/// ```
pub fn size(ftx: &FunctionContext, This(this): This<Value>) -> Result<i64> {
    let size = match this {
        Value::List(l) => l.len(),
        Value::Map(m) => m.map.len(),
        Value::String(s) => s.len(),
        Value::Bytes(b) => b.len(),
        value => return Err(ftx.error(format!("cannot determine the size of {:?}", value))),
    };
    Ok(size as i64)
}

/// Returns true if the target contains the provided argument. The actual behavior
/// depends mainly on the type of the target.
///
/// The following [`Value`] variants are supported:
/// * [`Value::List`] - Returns true if the list contains the provided value.
/// * [`Value::Map`] - Returns true if the map contains the provided key.
/// * [`Value::String`] - Returns true if the string contains the provided substring.
/// * [`Value::Bytes`] - Returns true if the bytes contain the provided byte.
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
pub fn contains(This(this): This<Value>, arg: Value) -> Result<Value> {
    Ok(match this {
        Value::List(v) => v.contains(&arg),
        Value::Map(v) => v
            .map
            .contains_key(&arg.try_into().map_err(ExecutionError::UnsupportedKeyType)?),
        Value::String(s) => {
            if let Value::String(arg) = arg {
                s.contains(arg.as_str())
            } else {
                false
            }
        }
        Value::Bytes(b) => {
            if let Value::Bytes(arg) = arg {
                let s = arg.as_slice();
                b.windows(arg.len()).any(|w| w == s)
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
// * `duration` - Returns the duration in a string formatted like "72h3m0.5s".
// * `int` - Returns the integer value of the target.
// * `uint` - Returns the unsigned integer value of the target.
// * `float` - Returns the float value of the target.
// * `bytes` - Converts bytes to string using from_utf8_lossy.
pub fn string(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => Value::String(v.clone()),
        #[cfg(feature = "chrono")]
        Value::Timestamp(t) => Value::String(t.to_rfc3339().into()),
        #[cfg(feature = "chrono")]
        Value::Duration(v) => Value::String(crate::duration::format_duration(&v).into()),
        Value::Int(v) => Value::String(v.to_string().into()),
        Value::UInt(v) => Value::String(v.to_string().into()),
        Value::Float(v) => Value::String(v.to_string().into()),
        Value::Bytes(v) => Value::String(Arc::new(String::from_utf8_lossy(v.as_slice()).into())),
        v => return Err(ftx.error(format!("cannot convert {:?} to string", v))),
    })
}

pub fn bytes(value: Arc<String>) -> Result<Value> {
    Ok(Value::Bytes(value.as_bytes().to_vec().into()))
}

// Performs a type conversion on the target.
pub fn double(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => v
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|e| ftx.error(format!("string parse error: {e}")))?,
        Value::Float(v) => Value::Float(v),
        Value::Int(v) => Value::Float(v as f64),
        Value::UInt(v) => Value::Float(v as f64),
        v => return Err(ftx.error(format!("cannot convert {:?} to double", v))),
    })
}

// Performs a type conversion on the target.
pub fn uint(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => v
            .parse::<u64>()
            .map(Value::UInt)
            .map_err(|e| ftx.error(format!("string parse error: {e}")))?,
        Value::Float(v) => {
            if v > u64::MAX as f64 || v < u64::MIN as f64 {
                return Err(ftx.error("unsigned integer overflow"));
            }
            Value::UInt(v as u64)
        }
        Value::Int(v) => Value::UInt(
            v.try_into()
                .map_err(|_| ftx.error("unsigned integer overflow"))?,
        ),
        Value::UInt(v) => Value::UInt(v),
        v => return Err(ftx.error(format!("cannot convert {:?} to uint", v))),
    })
}

// Performs a type conversion on the target.
pub fn int(ftx: &FunctionContext, This(this): This<Value>) -> Result<Value> {
    Ok(match this {
        Value::String(v) => v
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|e| ftx.error(format!("string parse error: {e}")))?,
        Value::Float(v) => {
            if v > i64::MAX as f64 || v < i64::MIN as f64 {
                return Err(ftx.error("integer overflow"));
            }
            Value::Int(v as i64)
        }
        Value::Int(v) => Value::Int(v),
        Value::UInt(v) => Value::Int(v.try_into().map_err(|_| ftx.error("integer overflow"))?),
        v => return Err(ftx.error(format!("cannot convert {:?} to int", v))),
    })
}

/// Returns true if a string starts with another string.
///
/// # Example
/// ```cel
/// "abc".startsWith("a") == true
/// ```
pub fn starts_with(This(this): This<Arc<String>>, prefix: Arc<String>) -> bool {
    this.starts_with(prefix.as_str())
}

/// Returns true if a string ends with another string.
///
/// # Example
/// ```cel
/// "abc".endsWith("c") == true
/// ```
pub fn ends_with(This(this): This<Arc<String>>, suffix: Arc<String>) -> bool {
    this.ends_with(suffix.as_str())
}

/// Returns true if a string matches the regular expression.
///
/// # Example
/// ```cel
/// "abc".matches("^[a-z]*$") == true
/// ```
#[cfg(feature = "regex")]
pub fn matches(
    ftx: &FunctionContext,
    This(this): This<Arc<String>>,
    regex: Arc<String>,
) -> Result<bool> {
    match regex::Regex::new(&regex) {
        Ok(re) => Ok(re.is_match(&this)),
        Err(err) => Err(ftx.error(format!("'{regex}' not a valid regex:\n{err}"))),
    }
}

/// Returns true if the provided argument can be resolved.
///
/// This function is useful for checking if a property exists on a type before
/// attempting to resolve it. Resolving a property that does not exist will
/// result in a [`ExecutionError::NoSuchKey`] error.
///
/// Operates similar to the `has` macro describe in the Go CEL implementation
/// spec: <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>.
///
/// # Examples
/// ```cel
/// has(foo.bar.baz)
/// ```
pub fn has(ftx: &FunctionContext) -> Result<Value> {
    // We determine if a type has a property by attempting to resolve it.
    // If we get a NoSuchKey error, then we know the property does not exist
    match ftx.resolve(Argument(0)) {
        Ok(_) => Value::Bool(true),
        Err(err) => match err {
            ExecutionError::NoSuchKey(_) => Value::Bool(false),
            _ => return Err(err),
        },
    }
    .into()
}

/// Maps the provided list to a new list by applying an expression to each
/// input item.
///
/// This function is intended to be used like the CEL-go `map` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Examples
/// ```cel
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
/// ```
pub fn map(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<Value> {
    match this {
        Value::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.iter() {
                ptx.add_variable_from_value(ident.clone(), item.clone());
                let value = ptx.resolve(&expr)?;
                values.push(value);
            }
            Value::List(Arc::new(values))
        }
        Value::Map(map) => {
            let mut values = Vec::with_capacity(map.map.len());
            let mut ptx = ftx.ptx.new_inner_scope();
            for (key, _) in map.map.iter() {
                ptx.add_variable_from_value(ident.clone(), key.clone());
                let value = ptx.resolve(&expr)?;
                values.push(value);
            }
            Value::List(Arc::new(values))
        }
        _ => return Err(this.error_expected_type(ValueType::List)),
    }
    .into()
}

/// Filters the provided list by applying an expression to each input item
/// and including the input item in the resulting list, only if the expression
/// returned true.
///
/// This function is intended to be used like the CEL-go `filter` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].filter(x, x > 1) == [2, 3]
/// ```
pub fn filter(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<Value> {
    match this {
        Value::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.iter() {
                ptx.add_variable_from_value(ident.clone(), item.clone());
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    values.push(item.clone());
                }
            }
            Value::List(Arc::new(values))
        }
        _ => return Err(this.error_expected_type(ValueType::List)),
    }
    .into()
}

/// Returns a boolean value indicating whether every value in the provided
/// list or map met the predicate defined by the provided expression. If
/// called on a map, the predicate is applied to the map keys.
///
/// This function is intended to be used like the CEL-go `all` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].all(x, x > 0) == true
/// [{1:true, 2:true, 3:false}].all(x, x > 0) == true
/// ```
pub fn all(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<bool> {
    match this {
        Value::List(items) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.iter() {
                ptx.add_variable_from_value(&ident, item);
                if let Value::Bool(false) = ptx.resolve(&expr)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        Value::Map(value) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            for key in value.map.keys() {
                ptx.add_variable_from_value(&ident, key);
                if let Value::Bool(false) = ptx.resolve(&expr)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        _ => Err(this.error_expected_type(ValueType::List)),
    }
}

/// Returns a boolean value indicating whether a or more values in the provided
/// list or map meet the predicate defined by the provided expression.
///
/// If called on a map, the predicate is applied to the map keys.
///
/// This function is intended to be used like the CEL-go `exists` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].exists(x, x > 0) == true
/// [{1:true, 2:true, 3:false}].exists(x, x > 0) == true
/// ```
pub fn exists(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<bool> {
    match this {
        Value::List(items) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            for item in items.iter() {
                ptx.add_variable_from_value(&ident, item);
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        Value::Map(value) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            for key in value.map.keys() {
                ptx.add_variable_from_value(&ident, key);
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        _ => Err(this.error_expected_type(ValueType::List)),
    }
}

/// Returns a boolean value indicating whether only one value in the provided
/// list or map meets the predicate defined by the provided expression.
///
/// If called on a map, the predicate is applied to the map keys.
///
/// This function is intended to be used like the CEL-go `exists` macro:
/// <https://github.com/google/cel-spec/blob/master/doc/langdef.md#macros>
///
/// # Example
/// ```cel
/// [1, 2, 3].exists_one(x, x > 0) == false
/// [1, 2, 3].exists_one(x, x == 1) == true
/// [{1:true, 2:true, 3:false}].exists_one(x, x > 0) == false
/// ```
pub fn exists_one(
    ftx: &FunctionContext,
    This(this): This<Value>,
    ident: Identifier,
    expr: Expression,
) -> Result<bool> {
    match this {
        Value::List(items) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            let mut exists = false;
            for item in items.iter() {
                ptx.add_variable_from_value(&ident, item);
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    if exists {
                        return Ok(false);
                    }
                    exists = true;
                }
            }
            Ok(exists)
        }
        Value::Map(value) => {
            let mut ptx = ftx.ptx.new_inner_scope();
            let mut exists = false;
            for key in value.map.keys() {
                ptx.add_variable_from_value(&ident, key);
                if let Value::Bool(true) = ptx.resolve(&expr)? {
                    if exists {
                        return Ok(false);
                    }
                    exists = true;
                }
            }
            Ok(exists)
        }
        _ => Err(this.error_expected_type(ValueType::List)),
    }
}

#[cfg(feature = "chrono")]
pub use time::duration;
#[cfg(feature = "chrono")]
pub use time::timestamp;

#[cfg(feature = "chrono")]
pub mod time {
    use super::Result;
    use crate::magic::This;
    use crate::{ExecutionError, Value};
    use chrono::{Datelike, Days, Months, Timelike};
    use std::sync::Arc;

    /// Duration parses the provided argument into a [`Value::Duration`] value.
    ///
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
    pub fn duration(value: Arc<String>) -> crate::functions::Result<Value> {
        Ok(Value::Duration(_duration(value.as_str())?))
    }

    /// Timestamp parses the provided argument into a [`Value::Timestamp`] value.
    /// The
    pub fn timestamp(value: Arc<String>) -> Result<Value> {
        Ok(Value::Timestamp(
            chrono::DateTime::parse_from_rfc3339(value.as_str())
                .map_err(|e| ExecutionError::function_error("timestamp", e.to_string().as_str()))?,
        ))
    }

    /// A wrapper around [`parse_duration`] that converts errors into [`ExecutionError`].
    /// and only returns the duration, rather than returning the remaining input.
    fn _duration(i: &str) -> Result<chrono::Duration> {
        let (_, duration) = crate::duration::parse_duration(i)
            .map_err(|e| ExecutionError::function_error("duration", e.to_string()))?;
        Ok(duration)
    }

    fn _timestamp(i: &str) -> Result<chrono::DateTime<chrono::FixedOffset>> {
        chrono::DateTime::parse_from_rfc3339(i)
            .map_err(|e| ExecutionError::function_error("timestamp", e.to_string()))
    }

    pub fn timestamp_year(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok(this.year().into())
    }

    pub fn timestamp_month(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.month0() as i32).into())
    }

    pub fn timestamp_year_day(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        let year = this
            .checked_sub_days(Days::new(this.day0() as u64))
            .unwrap()
            .checked_sub_months(Months::new(this.month0()))
            .unwrap();
        Ok(this.signed_duration_since(year).num_days().into())
    }

    pub fn timestamp_month_day(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.day0() as i32).into())
    }

    pub fn timestamp_date(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.day() as i32).into())
    }

    pub fn timestamp_weekday(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.weekday().num_days_from_sunday() as i32).into())
    }

    pub fn timestamp_hours(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.hour() as i32).into())
    }

    pub fn timestamp_minutes(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.minute() as i32).into())
    }

    pub fn timestamp_seconds(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.second() as i32).into())
    }

    pub fn timestamp_millis(
        This(this): This<chrono::DateTime<chrono::FixedOffset>>,
    ) -> Result<Value> {
        Ok((this.timestamp_subsec_millis() as i32).into())
    }
}

pub fn max(Arguments(args): Arguments) -> Result<Value> {
    // If items is a list of values, then operate on the list
    let items = if args.len() == 1 {
        match &args[0] {
            Value::List(values) => values,
            _ => return Ok(args[0].clone()),
        }
    } else {
        &args
    };

    items
        .iter()
        .skip(1)
        .try_fold(items.first().unwrap_or(&Value::Null), |acc, x| {
            match acc.partial_cmp(x) {
                Some(Ordering::Greater) => Ok(acc),
                Some(_) => Ok(x),
                None => Err(ExecutionError::ValuesNotComparable(acc.clone(), x.clone())),
            }
        })
        .cloned()
}

pub fn min(Arguments(args): Arguments) -> Result<Value> {
    // If items is a list of values, then operate on the list
    let items = if args.len() == 1 {
        match &args[0] {
            Value::List(values) => values,
            _ => return Ok(args[0].clone()),
        }
    } else {
        &args
    };

    items
        .iter()
        .skip(1)
        .try_fold(items.first().unwrap_or(&Value::Null), |acc, x| {
            match acc.partial_cmp(x) {
                Some(Ordering::Less) => Ok(acc),
                Some(_) => Ok(x),
                None => Err(ExecutionError::ValuesNotComparable(acc.clone(), x.clone())),
            }
        })
        .cloned()
}

#[cfg(test)]
mod tests {
    use crate::context::Context;
    use crate::tests::test_script;

    fn assert_script(input: &(&str, &str)) {
        assert_eq!(test_script(input.1, None), Ok(true.into()), "{}", input.0);
    }

    #[test]
    fn test_size() {
        [
            ("size of list", "size([1, 2, 3]) == 3"),
            ("size of map", "size({'a': 1, 'b': 2, 'c': 3}) == 3"),
            ("size of string", "size('foo') == 3"),
            ("size of bytes", "size(b'foo') == 3"),
            ("size as a list method", "[1, 2, 3].size() == 3"),
            ("size as a string method", "'foobar'.size() == 6"),
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
            ctx.add_variable_from_value("foo", std::collections::HashMap::from([("bar", 1)]));
            assert_eq!(test_script(script, Some(ctx)), Ok(true.into()), "{}", name);
        }
    }

    #[test]
    fn test_map() {
        [
            ("map list", "[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
            ("map list 2", "[1, 2, 3].map(y, y + 1) == [2, 3, 4]"),
            (
                "nested map",
                "[[1, 2], [2, 3]].map(x, x.map(x, x * 2)) == [[2, 4], [4, 6]]",
            ),
            (
                "map to list",
                r#"{'John': 'smart'}.map(key, key) == ['John']"#,
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_filter() {
        [("filter list", "[1, 2, 3].filter(x, x > 2) == [3]")]
            .iter()
            .for_each(assert_script);
    }

    #[test]
    fn test_all() {
        [
            ("all list #1", "[0, 1, 2].all(x, x >= 0)"),
            ("all list #2", "[0, 1, 2].all(x, x > 0) == false"),
            ("all map", "{0: 0, 1:1, 2:2}.all(x, x >= 0) == true"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_exists() {
        [
            ("exist list #1", "[0, 1, 2].exists(x, x > 0)"),
            ("exist list #2", "[0, 1, 2].exists(x, x == 3) == false"),
            ("exist list #3", "[0, 1, 2, 2].exists(x, x == 2)"),
            ("exist map", "{0: 0, 1:1, 2:2}.exists(x, x > 0)"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_exists_one() {
        [
            ("exist list #1", "[0, 1, 2].exists_one(x, x > 0) == false"),
            ("exist list #2", "[0, 1, 2].exists_one(x, x == 0)"),
            ("exist map", "{0: 0, 1:1, 2:2}.exists_one(x, x == 2)"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_max() {
        [
            ("max single", "max(1) == 1"),
            ("max multiple", "max(1, 2, 3) == 3"),
            ("max negative", "max(-1, 0) == 0"),
            ("max float", "max(-1.0, 0.0) == 0.0"),
            ("max list", "max([1, 2, 3]) == 3"),
            ("max empty list", "max([]) == null"),
            ("max no args", "max() == null"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_min() {
        [
            ("min single", "min(1) == 1"),
            ("min multiple", "min(1, 2, 3) == 1"),
            ("min negative", "min(-1, 0) == -1"),
            ("min float", "min(-1.0, 0.0) == -1.0"),
            (
                "min float multiple",
                "min(1.61803, 3.1415, 2.71828, 1.41421) == 1.41421",
            ),
            ("min list", "min([1, 2, 3]) == 1"),
            ("min empty list", "min([]) == null"),
            ("min no args", "min() == null"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_starts_with() {
        [
            ("starts with true", "'foobar'.startsWith('foo') == true"),
            ("starts with false", "'foobar'.startsWith('bar') == false"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_ends_with() {
        [
            ("ends with true", "'foobar'.endsWith('bar') == true"),
            ("ends with false", "'foobar'.endsWith('foo') == false"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_timestamp() {
        [(
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
            (
                "timestamp getFullYear",
                "timestamp('2023-05-28T00:00:00Z').getFullYear() == 2023",
            ),
            (
                "timestamp getMonth",
                "timestamp('2023-05-28T00:00:00Z').getMonth() == 4",
            ),
            (
                "timestamp getDayOfMonth",
                "timestamp('2023-05-28T00:00:00Z').getDayOfMonth() == 27",
            ),
            (
                "timestamp getDayOfYear",
                "timestamp('2023-05-28T00:00:00Z').getDayOfYear() == 147",
            ),
            (
                "timestamp getDate",
                "timestamp('2023-05-28T00:00:00Z').getDate() == 28",
            ),
            (
                "timestamp getDayOfWeek",
                "timestamp('2023-05-28T00:00:00Z').getDayOfWeek() == 0",
            ),
            (
                "timestamp getHours",
                "timestamp('2023-05-28T02:00:00Z').getHours() == 2",
            ),
            (
                "timestamp getMinutes",
                " timestamp('2023-05-28T00:05:00Z').getMinutes() == 5",
            ),
            (
                "timestamp getSeconds",
                "timestamp('2023-05-28T00:00:06Z').getSeconds() == 6",
            ),
            (
                "timestamp getMilliseconds",
                "timestamp('2023-05-28T00:00:42.123Z').getMilliseconds() == 123",
            ),

        ]
        .iter()
        .for_each(assert_script);
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_duration() {
        [
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

    #[cfg(feature = "chrono")]
    #[test]
    fn test_timestamp_variable() {
        let mut context = Context::default();
        let ts: chrono::DateTime<chrono::FixedOffset> =
            chrono::DateTime::parse_from_rfc3339("2023-05-29T00:00:00Z").unwrap();
        context
            .add_variable("ts", crate::Value::Timestamp(ts))
            .unwrap();

        let program = crate::Program::compile("ts == timestamp('2023-05-29T00:00:00Z')").unwrap();
        let result = program.execute(&context).unwrap();
        assert_eq!(result, true.into());
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_chrono_string() {
        [
            ("duration", "duration('1h30m').string() == '1h30m0s'"),
            (
                "timestamp",
                "timestamp('2023-05-29T00:00:00Z').string() == '2023-05-29T00:00:00+00:00'",
            ),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_contains() {
        let tests = vec![
            ("list", "[1, 2, 3].contains(3) == true"),
            ("map", "{1: true, 2: true, 3: true}.contains(3) == true"),
            ("string", "'foobar'.contains('bar') == true"),
            ("bytes", "b'foobar'.contains(b'o') == true"),
        ];

        for (name, script) in tests {
            assert_eq!(test_script(script, None), Ok(true.into()), "{}", name);
        }
    }

    #[cfg(feature = "regex")]
    #[test]
    fn test_matches() {
        let tests = vec![
            ("string", "'foobar'.matches('^[a-zA-Z]*$') == true"),
            (
                "map",
                "{'1': 'abc', '2': 'def', '3': 'ghi'}.all(key, key.matches('^[a-zA-Z]*$')) == false",
            ),
        ];

        for (name, script) in tests {
            assert_eq!(
                test_script(script, None),
                Ok(true.into()),
                ".matches failed for '{name}'"
            );
        }
    }

    #[cfg(feature = "regex")]
    #[test]
    fn test_matches_err() {
        assert_eq!(
            test_script(
                "'foobar'.matches('(foo') == true", None),
            Err(
                crate::ExecutionError::FunctionError {
                    function: "matches".to_string(),
                    message: "'(foo' not a valid regex:\nregex parse error:\n    (foo\n    ^\nerror: unclosed group".to_string()
                }
            )
        );
    }

    #[test]
    fn test_string() {
        [
            ("string", "'foo'.string() == 'foo'"),
            ("int", "10.string() == '10'"),
            ("float", "10.5.string() == '10.5'"),
            ("bytes", "b'foo'.string() == 'foo'"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_bytes() {
        [
            ("string", "bytes('abc') == b'abc'"),
            ("bytes", "bytes('abc') == b'\\x61b\\x63'"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_double() {
        [
            ("string", "'10'.double() == 10.0"),
            ("int", "10.double() == 10.0"),
            ("double", "10.0.double() == 10.0"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_uint() {
        [
            ("string", "'10'.uint() == 10.uint()"),
            ("double", "10.5.uint() == 10.uint()"),
        ]
        .iter()
        .for_each(assert_script);
    }

    #[test]
    fn test_int() {
        [
            ("string", "'10'.int() == 10"),
            ("int", "10.int() == 10"),
            ("uint", "10.uint().int() == 10"),
            ("double", "10.5.int() == 10"),
        ]
        .iter()
        .for_each(assert_script);
    }
}
