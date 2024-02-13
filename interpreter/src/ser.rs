use crate::{objects::Key, Value};
use serde::{
    ser::{self, Impossible},
    Serialize,
};
use std::{collections::HashMap, fmt::Display, iter::FromIterator, sync::Arc};
use thiserror::Error;

pub struct Serializer;
pub struct KeySerializer;

#[derive(Error, Debug, PartialEq, Clone)]
pub enum SerializationError {
    InvalidKey(String),
}

impl ser::Error for SerializationError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        SerializationError::InvalidKey(msg.to_string())
    }
}

impl Display for SerializationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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

impl<'a> ser::Serializer for Serializer {
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
            map: HashMap::new(),
            next_key: None,
        })
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
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
    map: HashMap<Key, Value>,
    next_key: Option<Key>,
}

pub struct SerializeStructVariant {
    name: String,
    map: HashMap<Key, Value>,
}

impl<'a> ser::SerializeSeq for SerializeVec {
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

impl<'a> ser::SerializeTuple for SerializeVec {
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

impl<'a> ser::SerializeTupleStruct for SerializeVec {
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

impl<'a> ser::SerializeTupleVariant for SerializeTupleVariant {
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

impl<'a> ser::SerializeMap for SerializeMap {
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
        self.map.insert(
            self.next_key
                .clone()
                .expect("serialize_value called before serialize_key"),
            value.serialize(Serializer)?,
        );
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(self.map.into())
    }
}

impl<'a> ser::SerializeStruct for SerializeMap {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeMap::serialize_entry(self, key, value)
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeMap::end(self)
    }
}

impl<'a> ser::SerializeStructVariant for SerializeStructVariant {
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

impl<'a> ser::Serializer for KeySerializer {
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
