// The serde_json crate implements a Serializer for its own Value enum, that is
// almost exactly the same to our Value enum, so this is more or less copied
// from [serde_json](https://github.com/serde-rs/json/blob/master/src/value/ser.rs),
// also mentioned in the [serde documentation](https://serde.rs/).

use crate::{objects::Key, Value};
use chrono::FixedOffset;
use serde::{
    ser::{self, Impossible},
    Serialize,
};
use std::{collections::HashMap, fmt::Display, iter::FromIterator, sync::Arc};
use thiserror::Error;
pub struct Serializer;
pub struct KeySerializer;

/// A special Duration type which allows conversion to [Value::Duration] for
/// types using automatic conversion with [serde::Serialize].
///
/// It is only recommended to use this type with the cel_interpreter
/// [serde::Serializer] implementation, as it may produce unexpected output
/// with other Serializers.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Duration(pub chrono::Duration);

impl Duration {
    // Since serde can't natively represent durations, we serialize a special
    // struct+field to indicate we want to rebuild the duration in the result.
    const SECONDS_FIELD: &str = "$__cel_private_duration_secs";
    const NANOS_FIELD: &str = "$__cel_private_duration_nanos";
    const NAME: &str = "$__cel_private_Duration";
}

impl From<Duration> for chrono::Duration {
    fn from(value: Duration) -> Self {
        value.0
    }
}

impl From<chrono::Duration> for Duration {
    fn from(value: chrono::Duration) -> Self {
        Self(value)
    }
}

impl ser::Serialize for Duration {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        use serde::ser::SerializeStruct;

        let mut s = serializer.serialize_struct(Self::NAME, 1)?;
        s.serialize_field(Self::NANOS_FIELD, &self.0.subsec_nanos())?;
        s.serialize_field(Self::SECONDS_FIELD, &self.0.num_seconds())?;
        s.end()
    }
}

/// A special Timestamp type which allows conversion to [Value::Timestamp] for
/// types using automatic conversion with [serde::Serialize].
///
/// It is only recommended to use this type with the cel_interpreter
/// [serde::Serializer] implementation, as it may produce unexpected output
/// with other Serializers.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Timestamp(pub chrono::DateTime<FixedOffset>);

impl Timestamp {
    // Since serde can't natively represent durations, we serialize a special
    // struct+field to indicate we want to rebuild the duration in the result.
    const FIELD: &str = "$__cel_private_timestamp";
    const NAME: &str = "$__cel_private_Timestamp";
}

impl From<Timestamp> for chrono::DateTime<FixedOffset> {
    fn from(value: Timestamp) -> Self {
        value.0
    }
}

impl From<chrono::DateTime<FixedOffset>> for Timestamp {
    fn from(value: chrono::DateTime<FixedOffset>) -> Self {
        Self(value)
    }
}

impl ser::Serialize for Timestamp {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        use serde::ser::SerializeStruct;

        let mut s = serializer.serialize_struct(Self::NAME, 1)?;
        s.serialize_field(Self::FIELD, &self.0.to_rfc3339())?;
        s.end()
    }
}

#[derive(Error, Debug, PartialEq, Clone)]
pub enum SerializationError {
    InvalidKey(String),
    SerdeError(String),
}

impl ser::Error for SerializationError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        SerializationError::SerdeError(msg.to_string())
    }
}

impl Display for SerializationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SerializationError::SerdeError(msg) => formatter.write_str(msg),
            SerializationError::InvalidKey(msg) => formatter.write_str(msg),
        }
    }
}

pub type Result<T> = std::result::Result<T, SerializationError>;

pub fn to_value<T>(value: T) -> Result<Value>
where
    T: Serialize,
{
    value.serialize(Serializer)
}

impl ser::Serializer for Serializer {
    type Ok = Value;
    type Error = SerializationError;

    type SerializeSeq = SerializeVec;
    type SerializeTuple = SerializeVec;
    type SerializeTupleStruct = SerializeVec;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeMap;
    type SerializeStructVariant = SerializeStructVariant;

    fn serialize_bool(self, v: bool) -> Result<Value> {
        Ok(Value::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i16(self, v: i16) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i32(self, v: i32) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i64(self, v: i64) -> Result<Value> {
        Ok(Value::Int(v))
    }

    fn serialize_u8(self, v: u8) -> Result<Value> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u16(self, v: u16) -> Result<Value> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u32(self, v: u32) -> Result<Value> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u64(self, v: u64) -> Result<Value> {
        Ok(Value::UInt(v))
    }

    fn serialize_f32(self, v: f32) -> Result<Value> {
        self.serialize_f64(f64::from(v))
    }

    fn serialize_f64(self, v: f64) -> Result<Value> {
        Ok(Value::Float(v))
    }

    fn serialize_char(self, v: char) -> Result<Value> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Value> {
        Ok(Value::String(Arc::new(v.to_string())))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Value> {
        Ok(Value::Bytes(Arc::new(v.to_vec())))
    }

    fn serialize_none(self) -> Result<Value> {
        self.serialize_unit()
    }

    fn serialize_some<T>(self, value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Value> {
        Ok(Value::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Value> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        Ok(HashMap::from_iter([(variant.to_string(), value.serialize(Serializer)?)]).into())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SerializeVec {
            vec: Vec::with_capacity(_len.unwrap_or(0)),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(SerializeTupleVariant {
            name: String::from(variant),
            vec: Vec::with_capacity(_len),
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(SerializeMap {
            kind: SerializeMapKind::Map(HashMap::new()),
            next_key: None,
        })
    }

    fn serialize_struct(self, name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        match name {
            Duration::NAME => Ok(SerializeMap {
                kind: SerializeMapKind::Duration {
                    seconds: 0,
                    nanos: 0,
                },
                next_key: None,
            }),
            Timestamp::NAME => Ok(SerializeMap {
                kind: SerializeMapKind::Timestamp(Arc::new(String::new())),
                next_key: None,
            }),
            _ => self.serialize_map(Some(len)),
        }
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(SerializeStructVariant {
            name: String::from(variant),
            map: HashMap::new(),
        })
    }
}

pub struct SerializeVec {
    vec: Vec<Value>,
}

pub struct SerializeTupleVariant {
    name: String,
    vec: Vec<Value>,
}

pub struct SerializeMap {
    kind: SerializeMapKind,
    next_key: Option<Key>,
}

enum SerializeMapKind {
    Map(HashMap<Key, Value>),
    Duration { seconds: i64, nanos: i32 },
    Timestamp(Arc<String>),
}

pub struct SerializeStructVariant {
    name: String,
    map: HashMap<Key, Value>,
}

impl ser::SerializeSeq for SerializeVec {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push(to_value(value)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::List(Arc::new(self.vec)))
    }
}

impl ser::SerializeTuple for SerializeVec {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleStruct for SerializeVec {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleVariant for SerializeTupleVariant {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push(to_value(value)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        let map = HashMap::from_iter([(self.name, Arc::new(self.vec))]);
        Ok(map.into())
    }
}

impl ser::SerializeMap for SerializeMap {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.next_key = Some(key.serialize(KeySerializer)?);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let SerializeMapKind::Map(map) = &mut self.kind else {
            unreachable!();
        };
        map.insert(
            self.next_key.clone().ok_or_else(|| {
                SerializationError::InvalidKey(
                    "serialize_value called before serialize_key".to_string(),
                )
            })?,
            value.serialize(Serializer)?,
        );
        Ok(())
    }

    fn end(self) -> Result<Value> {
        match self.kind {
            SerializeMapKind::Map(map) => Ok(map.into()),
            SerializeMapKind::Duration { seconds, nanos } => Ok(chrono::Duration::seconds(seconds)
                .checked_add(&chrono::Duration::nanoseconds(nanos.into()))
                .unwrap()
                .into()),
            SerializeMapKind::Timestamp(raw) => Ok(chrono::DateTime::parse_from_rfc3339(&raw)
                .map_err(|e| SerializationError::SerdeError(e.to_string()))?
                .into()),
        }
    }
}

impl ser::SerializeStruct for SerializeMap {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        match &mut self.kind {
            SerializeMapKind::Map(_) => serde::ser::SerializeMap::serialize_entry(self, key, value),
            SerializeMapKind::Duration { seconds, nanos } => match key {
                Duration::SECONDS_FIELD => {
                    let Value::Int(val) = value.serialize(Serializer)? else {
                        return Err(SerializationError::SerdeError(
                            "invalid type of value in timestamp struct".to_owned(),
                        ));
                    };
                    *seconds = val;
                    Ok(())
                }
                Duration::NANOS_FIELD => {
                    let Value::Int(val) = value.serialize(Serializer)? else {
                        return Err(SerializationError::SerdeError(
                            "invalid type of value in timestamp struct".to_owned(),
                        ));
                    };
                    *nanos = val.try_into().map_err(|_| {
                        SerializationError::SerdeError(
                            "timestamp struct nanos field is invalid".to_owned(),
                        )
                    })?;
                    Ok(())
                }
                _ => Err(SerializationError::SerdeError(
                    "invalid field in duration struct".to_owned(),
                )),
            },
            SerializeMapKind::Timestamp(raw) => {
                if key != Timestamp::FIELD {
                    return Err(SerializationError::SerdeError(
                        "invalid field in timestamp struct".to_owned(),
                    ));
                }
                let Value::String(val) = value.serialize(Serializer)? else {
                    return Err(SerializationError::SerdeError(
                        "invalid type of value in timestamp struct".to_owned(),
                    ));
                };
                *raw = val;
                Ok(())
            }
        }
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeMap::end(self)
    }
}

impl ser::SerializeStructVariant for SerializeStructVariant {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.map
            .insert(key.serialize(KeySerializer)?, to_value(value)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        let map: HashMap<String, Value> = HashMap::from_iter([(self.name, self.map.into())]);
        Ok(map.into())
    }
}

impl ser::Serializer for KeySerializer {
    type Ok = Key;
    type Error = SerializationError;

    type SerializeSeq = Impossible<Key, SerializationError>;
    type SerializeTuple = Impossible<Key, SerializationError>;
    type SerializeTupleStruct = Impossible<Key, SerializationError>;
    type SerializeTupleVariant = Impossible<Key, SerializationError>;
    type SerializeMap = Impossible<Key, SerializationError>;
    type SerializeStruct = Impossible<Key, SerializationError>;
    type SerializeStructVariant = Impossible<Key, SerializationError>;

    fn serialize_bool(self, v: bool) -> Result<Key> {
        Ok(Key::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Key> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i16(self, v: i16) -> Result<Key> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i32(self, v: i32) -> Result<Key> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i64(self, v: i64) -> Result<Key> {
        Ok(Key::Int(v))
    }

    fn serialize_u8(self, v: u8) -> Result<Key> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u16(self, v: u16) -> Result<Key> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u32(self, v: u32) -> Result<Key> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u64(self, v: u64) -> Result<Key> {
        Ok(Key::Uint(v))
    }

    fn serialize_f32(self, _v: f32) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Float is not supported".to_string(),
        ))
    }

    fn serialize_f64(self, _v: f64) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Float is not supported".to_string(),
        ))
    }

    fn serialize_char(self, v: char) -> Result<Key> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Key> {
        Ok(Key::String(Arc::new(v.to_string())))
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Bytes are not supported".to_string(),
        ))
    }

    fn serialize_none(self) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "None is not supported".to_string(),
        ))
    }

    fn serialize_some<T>(self, value: &T) -> Result<Key>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Null is not supported".to_string(),
        ))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Empty unit structs are not supported".to_string(),
        ))
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Key> {
        Ok(Key::String(Arc::new(variant.to_string())))
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Key>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(KeySerializer)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Key>
    where
        T: ?Sized + Serialize,
    {
        Err(SerializationError::InvalidKey(
            "Newtype variant is not supported".to_string(),
        ))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Err(SerializationError::InvalidKey(
            "Sequences are not supported".to_string(),
        ))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Err(SerializationError::InvalidKey(
            "Tuples are not supported".to_string(),
        ))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Err(SerializationError::InvalidKey(
            "Structs are not supported".to_string(),
        ))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Err(SerializationError::InvalidKey(
            "Tuple variants are not supported".to_string(),
        ))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Err(SerializationError::InvalidKey(
            "Map variants are not supported".to_string(),
        ))
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Err(SerializationError::InvalidKey(
            "Structs are not supported".to_string(),
        ))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Err(SerializationError::InvalidKey(
            "Struct variants are not supported".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::{Duration, Timestamp};
    use crate::{objects::Key, to_value, Value};
    use crate::{Context, Program};
    use serde::Serialize;
    use serde_bytes::Bytes;
    use std::{collections::HashMap, iter::FromIterator, sync::Arc};

    macro_rules! primitive_test {
        ($functionName:ident, $strValue: literal, $value: expr) => {
            #[test]
            fn $functionName() {
                let program = Program::compile($strValue).unwrap();
                let result = program.execute(&Context::default());
                assert_eq!(Value::from($value), result.unwrap());
            }
        };
    }

    primitive_test!(test_u64_zero, "0u", 0_u64);
    primitive_test!(test_i64_zero, "0", 0_i64);
    primitive_test!(test_f64_zero, "0.0", 0_f64);
    //primitive_test!(test_f64_zero, "0.", 0_f64); this test fails
    primitive_test!(test_bool_false, "false", false);
    primitive_test!(test_bool_true, "true", true);
    primitive_test!(test_string_empty, "\"\"", "");
    primitive_test!(test_string_non_empty, "\"test\"", "test");
    primitive_test!(test_byte_ones, r#"b"\001\001""#, vec!(1_u8, 1_u8));
    // primitive_test!(test_triple_double_quoted_string, #"r"""""""#, "");
    // primitive_test!(test_triple_single_quoted_string, "r''''''", "");
    primitive_test!(test_utf8_character_as_bytes, "b'ÿ'", vec!(195_u8, 191_u8));

    #[test]
    fn test_json_data_conversion() {
        #[derive(Serialize)]
        struct TestPrimitives {
            bool: bool,
            u8: u8,
            u16: u16,
            u32: u32,
            u64: u64,
            int8: i8,
            int16: i16,
            int32: i32,
            int64: i64,
            f32: f32,
            f64: f64,
            char: char,
            string: String,
            bytes: &'static Bytes,
        }

        let test = TestPrimitives {
            bool: true,
            int8: 8_i8,
            int16: 16_i16,
            int32: 32_i32,
            int64: 64_i64,
            u8: 8_u8,
            u16: 16_u16,
            u32: 32_u32,
            u64: 64_u64,
            f32: 0.32_f32,
            f64: 0.64_f64,
            char: 'a',
            string: "string".to_string(),
            bytes: Bytes::new(&[1_u8, 1_u8, 1_u8, 1_u8]),
        };

        let serialized = to_value(test).unwrap();
        let expected: Value = HashMap::from_iter([
            (Key::String(Arc::new("bool".to_string())), Value::Bool(true)),
            (Key::String(Arc::new("int8".to_string())), Value::Int(8)),
            (Key::String(Arc::new("int16".to_string())), Value::Int(16)),
            (Key::String(Arc::new("int32".to_string())), Value::Int(32)),
            (Key::String(Arc::new("int64".to_string())), Value::Int(64)),
            (Key::String(Arc::new("u8".to_string())), Value::UInt(8)),
            (Key::String(Arc::new("u16".to_string())), Value::UInt(16)),
            (Key::String(Arc::new("u32".to_string())), Value::UInt(32)),
            (Key::String(Arc::new("u64".to_string())), Value::UInt(64)),
            (
                Key::String(Arc::new("f32".to_string())),
                Value::Float(f64::from(0.32_f32)),
            ),
            (Key::String(Arc::new("f64".to_string())), Value::Float(0.64)),
            (
                Key::String(Arc::new("char".to_string())),
                Value::String(Arc::new("a".to_string())),
            ),
            (
                Key::String(Arc::new("string".to_string())),
                Value::String(Arc::new("string".to_string())),
            ),
            (
                Key::String(Arc::new("bytes".to_string())),
                Value::Bytes(Arc::new(vec![1, 1, 1, 1])),
            ),
        ])
        .into();

        // Test with CEL because iterator is not implemented for Value::Map
        let program = Program::compile(
            "expected.all(key, (has(serialized[key]) && (serialized[key] == expected[key])))",
        )
        .unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("serialized", serialized).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }

    #[derive(Serialize)]
    enum TestCompoundTypes {
        Unit,
        Newtype(u32),
        Wrapped(Option<u8>),
        Tuple(u32, u32),
        Struct {
            a: i32,
            nested: HashMap<bool, HashMap<String, Vec<String>>>,
            duration: Duration,
            timestamp: Timestamp,
        },
        Map(HashMap<String, &'static Bytes>),
    }
    #[test]
    fn test_unit() {
        let unit = to_value(TestCompoundTypes::Unit).unwrap();
        let expected: Value = "Unit".into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", unit).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }
    #[test]
    fn test_newtype() {
        let newtype = to_value(TestCompoundTypes::Newtype(32)).unwrap();
        let expected: Value = HashMap::from([("Newtype", Value::UInt(32))]).into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", newtype).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }
    #[test]
    fn test_options() {
        // Test Option serialization
        let wrapped = to_value(TestCompoundTypes::Wrapped(None)).unwrap();
        let expected: Value = HashMap::from([("Wrapped", Value::Null)]).into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", wrapped).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());

        let wrapped = to_value(TestCompoundTypes::Wrapped(Some(8))).unwrap();
        let expected: Value = HashMap::from([("Wrapped", Value::UInt(8))]).into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", wrapped).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }

    #[test]
    fn test_tuples() {
        // Test Tuple serialization
        let tuple = to_value(TestCompoundTypes::Tuple(12, 16)).unwrap();
        let expected: Value = HashMap::from([(
            "Tuple",
            Value::List(Arc::new(vec![12_u64.into(), 16_u64.into()])),
        )])
        .into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", tuple).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }

    #[test]
    fn test_structs() {
        // Test Struct serialization
        let test_struct = TestCompoundTypes::Struct {
            a: 32_i32,
            nested: HashMap::from_iter([(
                true,
                HashMap::from_iter([(
                    "Test".to_string(),
                    vec!["a".to_string(), "b".to_string(), "c".to_string()],
                )]),
            )]),
            duration: chrono::Duration::milliseconds(12345).into(),
            timestamp: chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                .unwrap()
                .into(),
        };
        let expected: Value = HashMap::<Key, Value>::from([(
            "Struct".into(),
            HashMap::<Key, Value>::from_iter([
                ("a".into(), 32_i32.into()),
                (
                    "nested".into(),
                    HashMap::<Key, Value>::from_iter([(
                        true.into(),
                        HashMap::<Key, Value>::from_iter([(
                            "Test".into(),
                            vec!["a".to_string(), "b".to_string(), "c".to_string()].into(),
                        )])
                        .into(),
                    )])
                    .into(),
                ),
                (
                    "duration".into(),
                    chrono::Duration::milliseconds(12345).into(),
                ),
                (
                    "timestamp".into(),
                    chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                        .unwrap()
                        .into(),
                ),
            ])
            .into(),
        )])
        .into();
        let program = Program::compile("expected.all(key, test[key] == expected[key])").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", test_struct).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());
    }

    #[test]
    fn test_maps() {
        // Test Map serialization
        let map = to_value(TestCompoundTypes::Map(
            HashMap::<String, &'static Bytes>::from_iter([(
                "Test".to_string(),
                Bytes::new(&[0_u8, 0_u8, 0_u8, 0_u8]),
            )]),
        ))
        .unwrap();
        let expected: Value = HashMap::from([(
            "Map",
            HashMap::<Key, Value>::from_iter([(
                "Test".into(),
                Value::Bytes(Arc::new(vec![0_u8, 0_u8, 0_u8, 0_u8])),
            )]),
        )])
        .into();
        assert_eq!(map, expected)
    }

    #[test]
    fn test_durations() {
        // Test Duration serialization
        let map = to_value([
            chrono::Duration::milliseconds(1527).into(),
            // Let's test chrono::Duration's particular handling around math
            // and negatives.
            chrono::Duration::milliseconds(-1527).into(),
            Duration(chrono::Duration::seconds(1) - chrono::Duration::nanoseconds(1000000001)),
            Duration(chrono::Duration::seconds(-1) + chrono::Duration::nanoseconds(1000000001)),
        ])
        .unwrap();
        let expected: Value = vec![
            chrono::Duration::milliseconds(1527),
            chrono::Duration::nanoseconds(-1527000000),
            chrono::Duration::nanoseconds(-1),
            chrono::Duration::nanoseconds(1),
        ]
        .into();
        assert_eq!(map, expected)
    }
}
