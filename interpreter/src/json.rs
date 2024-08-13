use crate::Value;
use thiserror::Error;

/// Error converting a CEL value to a JSON value.
#[derive(Debug, Clone, Error)]
#[error("unable to convert value to json: {0:?}")]
pub struct ConvertToJsonError<'a>(&'a Value);

impl Value {
    /// Converts a CEL value to a JSON value.
    ///
    /// # Example
    /// ```
    /// use cel_interpreter::{Context, Program};
    ///
    /// let program = Program::compile("null").unwrap();
    /// let value = program.execute(&Context::default()).unwrap();
    /// let result = value.json().unwrap();
    ///
    /// assert_eq!(result, serde_json::Value::Null);
    /// ```
    pub fn json(&self) -> Result<serde_json::Value, ConvertToJsonError> {
        use base64::prelude::*;
        Ok(match *self {
            Value::List(ref vec) => serde_json::Value::Array(
                vec.iter()
                    .map(|v| v.json())
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Value::Map(ref map) => {
                let mut obj = serde_json::Map::new();
                for (k, v) in map.map.iter() {
                    obj.insert(k.to_string(), v.json()?);
                }
                serde_json::Value::Object(obj)
            }
            Value::Int(i) => i.into(),
            Value::UInt(u) => u.into(),
            Value::Float(f) => f.into(),
            Value::String(ref s) => s.to_string().into(),
            Value::Bool(b) => b.into(),
            Value::Timestamp(ref dt) => dt.to_rfc3339().into(),
            Value::Bytes(ref b) => BASE64_STANDARD.encode(b.as_slice()).to_string().into(),
            Value::Null => serde_json::Value::Null,
            _ => return Err(ConvertToJsonError(self)),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::objects::Map;
    use crate::Value as CelValue;
    use serde_json::Value as SerdeValue;
    use std::collections::HashMap;

    #[test]
    fn test_cel_value_to_json() {
        let tests = [
            (
                SerdeValue::String("hello".to_string()),
                CelValue::String("hello".to_string().into()),
            ),
            (
                SerdeValue::Number(serde_json::Number::from(42)),
                CelValue::Int(42),
            ),
            (
                SerdeValue::Number(serde_json::Number::from_f64(42.0).unwrap()),
                CelValue::Float(42.0),
            ),
            (SerdeValue::Bool(true), CelValue::Bool(true)),
            (SerdeValue::Null, CelValue::Null),
            (
                SerdeValue::Array(vec![SerdeValue::Bool(true), SerdeValue::Null]),
                CelValue::List(vec![CelValue::Bool(true), CelValue::Null].into()),
            ),
            (
                SerdeValue::Object({
                    let mut obj = serde_json::Map::new();
                    obj.insert("hello".to_string(), SerdeValue::String("world".to_string()));
                    obj
                }),
                CelValue::Map(Map::from(HashMap::from([(
                    "hello".to_string(),
                    CelValue::String("world".to_string().into()),
                )]))),
            ),
        ];

        for (expected, value) in tests.iter() {
            assert_eq!(
                value.json().unwrap(),
                *expected,
                "{:?}={:?}",
                value,
                expected
            );
        }
    }
}