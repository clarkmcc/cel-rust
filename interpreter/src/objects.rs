use crate::context::Context;
use crate::functions::FunctionContext;
use crate::ser::SerializationError;
use crate::ExecutionError::NoSuchKey;
use crate::{to_value, ExecutionError};
use cel_parser::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};
use chrono::{DateTime, Duration, FixedOffset};
use core::ops;
use serde::{Serialize, Serializer};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::{Infallible, TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub struct Map {
    pub map: Arc<HashMap<Key, Value>>,
}

impl PartialOrd for Map {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}

impl Map {
    /// Returns a reference to the value corresponding to the key. Implicitly converts between int
    /// and uint keys.
    pub fn get(&self, key: &Key) -> Option<&Value> {
        self.map.get(key).or_else(|| {
            // Also check keys that are cross type comparable.
            let converted = match key {
                Key::Int(k) => Key::Uint(u64::try_from(*k).ok()?),
                Key::Uint(k) => Key::Int(i64::try_from(*k).ok()?),
                _ => return None,
            };
            self.map.get(&converted)
        })
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Ord, Clone, PartialOrd)]
pub enum Key {
    Int(i64),
    Uint(u64),
    Bool(bool),
    String(Arc<String>),
}

/// Implement conversions from primitive types to [`Key`]

impl From<String> for Key {
    fn from(v: String) -> Self {
        Key::String(v.into())
    }
}

impl From<Arc<String>> for Key {
    fn from(v: Arc<String>) -> Self {
        Key::String(v.clone())
    }
}

impl<'a> From<&'a str> for Key {
    fn from(v: &'a str) -> Self {
        Key::String(Arc::new(v.into()))
    }
}

impl From<bool> for Key {
    fn from(v: bool) -> Self {
        Key::Bool(v)
    }
}

impl From<i64> for Key {
    fn from(v: i64) -> Self {
        Key::Int(v)
    }
}

impl From<u64> for Key {
    fn from(v: u64) -> Self {
        Key::Uint(v)
    }
}

impl Serialize for Key {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Key::Int(v) => v.serialize(serializer),
            Key::Uint(v) => v.serialize(serializer),
            Key::Bool(v) => v.serialize(serializer),
            Key::String(v) => v.serialize(serializer),
        }
    }
}

/// Implement conversions from [`Key`] into [`Value`]

impl TryInto<Key> for Value {
    type Error = Value;

    #[inline(always)]
    fn try_into(self) -> Result<Key, Self::Error> {
        match self {
            Value::Int(v) => Ok(Key::Int(v)),
            Value::UInt(v) => Ok(Key::Uint(v)),
            Value::String(v) => Ok(Key::String(v)),
            Value::Bool(v) => Ok(Key::Bool(v)),
            _ => Err(self),
        }
    }
}

// Implement conversion from HashMap<K, V> into CelMap
impl<K: Into<Key>, V: Into<Value>> From<HashMap<K, V>> for Map {
    fn from(map: HashMap<K, V>) -> Self {
        let mut new_map = HashMap::new();
        for (k, v) in map {
            new_map.insert(k.into(), v.into());
        }
        Map {
            map: Arc::new(new_map),
        }
    }
}

pub trait TryIntoValue {
    type Error: std::error::Error + 'static;
    fn try_into_value(self) -> Result<Value, Self::Error>;
}

impl<T: Serialize> TryIntoValue for T {
    type Error = SerializationError;
    fn try_into_value(self) -> Result<Value, Self::Error> {
        to_value(self)
    }
}

impl TryIntoValue for Value {
    type Error = Infallible;
    fn try_into_value(self) -> Result<Value, Self::Error> {
        Ok(self)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    List(Arc<Vec<Value>>),
    Map(Map),

    Function(Arc<String>, Option<Box<Value>>),

    // Atoms
    Int(i64),
    UInt(u64),
    Float(f64),
    String(Arc<String>),
    Bytes(Arc<Vec<u8>>),
    Bool(bool),
    Duration(Duration),
    Timestamp(DateTime<FixedOffset>),
    Null,
}

pub enum ValueType {
    List,
    Map,
    Function,
    Int,
    UInt,
    Float,
    String,
    Bytes,
    Bool,
    Duration,
    Timestamp,
    Null,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::List => write!(f, "list"),
            ValueType::Map => write!(f, "map"),
            ValueType::Function => write!(f, "function"),
            ValueType::Int => write!(f, "int"),
            ValueType::UInt => write!(f, "uint"),
            ValueType::Float => write!(f, "float"),
            ValueType::String => write!(f, "string"),
            ValueType::Bytes => write!(f, "bytes"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Duration => write!(f, "duration"),
            ValueType::Timestamp => write!(f, "timestamp"),
            ValueType::Null => write!(f, "null"),
        }
    }
}

impl Value {
    pub fn type_of(&self) -> ValueType {
        match self {
            Value::List(_) => ValueType::List,
            Value::Map(_) => ValueType::Map,
            Value::Function(_, _) => ValueType::Function,
            Value::Int(_) => ValueType::Int,
            Value::UInt(_) => ValueType::UInt,
            Value::Float(_) => ValueType::Float,
            Value::String(_) => ValueType::String,
            Value::Bytes(_) => ValueType::Bytes,
            Value::Bool(_) => ValueType::Bool,
            Value::Duration(_) => ValueType::Duration,
            Value::Timestamp(_) => ValueType::Timestamp,
            Value::Null => ValueType::Null,
        }
    }

    pub fn error_expected_type(&self, expected: ValueType) -> ExecutionError {
        ExecutionError::UnexpectedType {
            got: self.type_of().to_string(),
            want: expected.to_string(),
        }
    }
}

impl From<&Value> for Value {
    fn from(value: &Value) -> Self {
        value.clone()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Function(a1, a2), Value::Function(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::UInt(a), Value::UInt(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Duration(a), Value::Duration(b)) => a == b,
            (Value::Timestamp(a), Value::Timestamp(b)) => a == b,
            // Allow different numeric types to be compared without explicit casting.
            (Value::Int(a), Value::UInt(b)) => a
                .to_owned()
                .try_into()
                .map(|a: u64| a == *b)
                .unwrap_or(false),
            (Value::Int(a), Value::Float(b)) => (*a as f64) == *b,
            (Value::UInt(a), Value::Int(b)) => a
                .to_owned()
                .try_into()
                .map(|a: i64| a == *b)
                .unwrap_or(false),
            (Value::UInt(a), Value::Float(b)) => (*a as f64) == *b,
            (Value::Float(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Float(a), Value::UInt(b)) => *a == (*b as f64),
            (_, _) => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Some(a.cmp(b)),
            (Value::UInt(a), Value::UInt(b)) => Some(a.cmp(b)),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => Some(a.cmp(b)),
            (Value::Bool(a), Value::Bool(b)) => Some(a.cmp(b)),
            (Value::Null, Value::Null) => Some(Ordering::Equal),
            (Value::Duration(a), Value::Duration(b)) => Some(a.cmp(b)),
            (Value::Timestamp(a), Value::Timestamp(b)) => Some(a.cmp(b)),
            // Allow different numeric types to be compared without explicit casting.
            (Value::Int(a), Value::UInt(b)) => Some(
                a.to_owned()
                    .try_into()
                    .map(|a: u64| a.cmp(b))
                    // If the i64 doesn't fit into a u64 it must be less than 0.
                    .unwrap_or(Ordering::Less),
            ),
            (Value::Int(a), Value::Float(b)) => (*a as f64).partial_cmp(b),
            (Value::UInt(a), Value::Int(b)) => Some(
                a.to_owned()
                    .try_into()
                    .map(|a: i64| a.cmp(b))
                    // If the u64 doesn't fit into a i64 it must be greater than i64::MAX.
                    .unwrap_or(Ordering::Greater),
            ),
            (Value::UInt(a), Value::Float(b)) => (*a as f64).partial_cmp(b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Value::Float(a), Value::UInt(b)) => a.partial_cmp(&(*b as f64)),
            _ => None,
        }
    }
}

impl From<&Key> for Value {
    fn from(value: &Key) -> Self {
        match value {
            Key::Int(v) => Value::Int(*v),
            Key::Uint(v) => Value::UInt(*v),
            Key::Bool(v) => Value::Bool(*v),
            Key::String(v) => Value::String(v.clone()),
        }
    }
}

impl From<Key> for Value {
    fn from(value: Key) -> Self {
        match value {
            Key::Int(v) => Value::Int(v),
            Key::Uint(v) => Value::UInt(v),
            Key::Bool(v) => Value::Bool(v),
            Key::String(v) => Value::String(v),
        }
    }
}

impl From<&Key> for Key {
    fn from(key: &Key) -> Self {
        key.clone()
    }
}

// Convert Vec<T> to Value
impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::List(v.into_iter().map(|v| v.into()).collect::<Vec<_>>().into())
    }
}

// Convert Vec<u8> to Value
impl From<Vec<u8>> for Value {
    fn from(v: Vec<u8>) -> Self {
        Value::Bytes(v.into())
    }
}

// Convert String to Value
impl From<String> for Value {
    fn from(v: String) -> Self {
        Value::String(v.into())
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Value::String(v.to_string().into())
    }
}

// Convert Option<T> to Value
impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(v: Option<T>) -> Self {
        match v {
            Some(v) => v.into(),
            None => Value::Null,
        }
    }
}

// Convert HashMap<K, V> to Value
impl<K: Into<Key>, V: Into<Value>> From<HashMap<K, V>> for Value {
    fn from(v: HashMap<K, V>) -> Self {
        Value::Map(v.into())
    }
}

impl From<ExecutionError> for ResolveResult {
    fn from(value: ExecutionError) -> Self {
        Err(value)
    }
}

pub type ResolveResult = Result<Value, ExecutionError>;

impl From<Value> for ResolveResult {
    fn from(value: Value) -> Self {
        Ok(value)
    }
}

impl<'a> Value {
    pub fn resolve_all(expr: &[Expression], ctx: &Context) -> ResolveResult {
        let mut res = Vec::with_capacity(expr.len());
        for expr in expr {
            res.push(Value::resolve(expr, ctx)?);
        }
        Ok(Value::List(res.into()))
    }

    #[inline(always)]
    pub fn resolve(expr: &'a Expression, ctx: &Context) -> ResolveResult {
        match expr {
            Expression::Atom(atom) => Ok(atom.into()),
            Expression::Arithmetic(left, op, right) => {
                let left = Value::resolve(left, ctx)?;
                let right = Value::resolve(right, ctx)?;

                match op {
                    ArithmeticOp::Add => left + right,
                    ArithmeticOp::Subtract => left - right,
                    ArithmeticOp::Divide => left / right,
                    ArithmeticOp::Multiply => left * right,
                    ArithmeticOp::Modulus => left % right,
                }
            }
            Expression::Relation(left, op, right) => {
                let left = Value::resolve(left, ctx)?;
                let right = Value::resolve(right, ctx)?;
                let res = match op {
                    RelationOp::LessThan => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            == Ordering::Less
                    }
                    RelationOp::LessThanEq => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            != Ordering::Greater
                    }
                    RelationOp::GreaterThan => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            == Ordering::Greater
                    }
                    RelationOp::GreaterThanEq => {
                        left.partial_cmp(&right)
                            .ok_or(ExecutionError::ValuesNotComparable(left, right))?
                            != Ordering::Less
                    }
                    RelationOp::Equals => right.eq(&left),
                    RelationOp::NotEquals => right.ne(&left),
                    RelationOp::In => match (left, right) {
                        (Value::String(l), Value::String(r)) => r.contains(&*l),
                        (any, Value::List(v)) => v.contains(&any),
                        (any, Value::Map(m)) => match any.try_into() {
                            Ok(key) => m.map.contains_key(&key),
                            Err(_) => false,
                        },
                        (left, right) => Err(ExecutionError::ValuesNotComparable(left, right))?,
                    },
                };
                Value::Bool(res).into()
            }
            Expression::Ternary(cond, left, right) => {
                let cond = Value::resolve(cond, ctx)?;
                if cond.to_bool() {
                    Value::resolve(left, ctx)
                } else {
                    Value::resolve(right, ctx)
                }
            }
            Expression::Or(left, right) => {
                let left = Value::resolve(left, ctx)?;
                if left.to_bool() {
                    left.into()
                } else {
                    Value::resolve(right, ctx)
                }
            }
            Expression::And(left, right) => {
                let left = Value::resolve(left, ctx)?;
                let right = Value::resolve(right, ctx)?;
                Value::Bool(left.to_bool() && right.to_bool()).into()
            }
            Expression::Unary(op, expr) => {
                let expr = Value::resolve(expr, ctx)?;
                match op {
                    UnaryOp::Not => Ok(Value::Bool(!expr.to_bool())),
                    UnaryOp::DoubleNot => Ok(Value::Bool(expr.to_bool())),
                    UnaryOp::Minus => match expr {
                        Value::Int(i) => Ok(Value::Int(-i)),
                        Value::Float(i) => Ok(Value::Float(-i)),
                        value => Err(ExecutionError::UnsupportedUnaryOperator("minus", value)),
                    },
                    UnaryOp::DoubleMinus => match expr {
                        Value::Int(_) => Ok(expr),
                        Value::UInt(_) => Ok(expr),
                        Value::Float(_) => Ok(expr),
                        value => Err(ExecutionError::UnsupportedUnaryOperator("negate", value)),
                    },
                }
            }
            Expression::Member(left, right) => {
                let left = Value::resolve(left, ctx)?;
                left.member(right, ctx)
            }
            Expression::List(items) => {
                let list = items
                    .iter()
                    .map(|i| Value::resolve(i, ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Value::List(list.into()).into()
            }
            Expression::Map(items) => {
                let mut map = HashMap::default();
                for (k, v) in items.iter() {
                    let key = Value::resolve(k, ctx)?
                        .try_into()
                        .map_err(ExecutionError::UnsupportedKeyType)?;
                    let value = Value::resolve(v, ctx)?;
                    map.insert(key, value);
                }
                Value::Map(Map {
                    map: Arc::from(map),
                })
                .into()
            }
            Expression::Ident(name) => ctx.get_variable(&***name),
            Expression::FunctionCall(name, target, args) => {
                if let Expression::Ident(name) = &**name {
                    let func = ctx
                        .get_function(&**name)
                        .ok_or_else(|| ExecutionError::UndeclaredReference(Arc::clone(name)))?;
                    match target {
                        None => {
                            let mut ctx =
                                FunctionContext::new(name.clone(), None, ctx, args.clone());
                            func.call_with_context(&mut ctx)
                        }
                        Some(target) => {
                            let mut ctx = FunctionContext::new(
                                name.clone(),
                                Some(Value::resolve(target, ctx)?),
                                ctx,
                                args.clone(),
                            );
                            func.call_with_context(&mut ctx)
                        }
                    }
                } else {
                    Err(ExecutionError::UnsupportedFunctionCallIdentifierType(
                        (**name).clone(),
                    ))
                }
            }
        }
    }

    // >> a(b)
    // Member(Ident("a"),
    //        FunctionCall([Ident("b")]))
    // >> a.b(c)
    // Member(Member(Ident("a"),
    //               Attribute("b")),
    //        FunctionCall([Ident("c")]))

    fn member(self, member: &Member, ctx: &Context) -> ResolveResult {
        match member {
            Member::Index(idx) => {
                let idx = Value::resolve(idx, ctx)?;
                match (self, idx) {
                    (Value::List(items), Value::Int(idx)) => items
                        .get(idx as usize)
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::String(str), Value::Int(idx)) => {
                        match str.get(idx as usize..(idx + 1) as usize) {
                            None => Value::Null,
                            Some(str) => Value::String(str.to_string().into()),
                        }
                        .into()
                    }
                    (Value::Map(map), Value::String(property)) => map
                        .get(&property.into())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::Map(map), Value::Bool(property)) => map
                        .get(&property.into())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::Map(map), Value::Int(property)) => map
                        .get(&property.into())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::Map(map), Value::UInt(property)) => map
                        .get(&property.into())
                        .cloned()
                        .unwrap_or(Value::Null)
                        .into(),
                    (Value::Map(_), index) => Err(ExecutionError::UnsupportedMapIndex(index)),
                    (Value::List(_), index) => Err(ExecutionError::UnsupportedListIndex(index)),
                    (value, index) => Err(ExecutionError::UnsupportedIndex(value, index)),
                }
            }
            Member::Fields(_) => Err(ExecutionError::UnsupportedFieldsConstruction(
                member.clone(),
            )),
            Member::Attribute(name) => {
                // This will always either be because we're trying to access
                // a property on self, or a method on self.
                let child = match self {
                    Value::Map(ref m) => m.map.get(&name.clone().into()).cloned(),
                    _ => None,
                };

                // If the property is both an attribute and a method, then we
                // give priority to the property. Maybe we can implement lookahead
                // to see if the next token is a function call?
                match (child, ctx.has_function(&***name)) {
                    (None, false) => NoSuchKey(name.clone()).into(),
                    (Some(child), _) => child.into(),
                    (None, true) => Value::Function(name.clone(), Some(self.into())).into(),
                }
            }
        }
    }

    #[inline(always)]
    fn to_bool(&self) -> bool {
        match self {
            Value::List(v) => !v.is_empty(),
            Value::Map(v) => !v.map.is_empty(),
            Value::Int(v) => *v != 0,
            Value::UInt(v) => *v != 0,
            Value::Float(v) => *v != 0.0,
            Value::String(v) => !v.is_empty(),
            Value::Bytes(v) => !v.is_empty(),
            Value::Bool(v) => *v,
            Value::Null => false,
            Value::Duration(v) => v.num_nanoseconds().map(|n| n != 0).unwrap_or(false),
            Value::Timestamp(v) => v.timestamp_nanos_opt().unwrap_or_default() > 0,
            Value::Function(_, _) => false,
        }
    }
}

impl From<&Atom> for Value {
    #[inline(always)]
    fn from(atom: &Atom) -> Self {
        match atom {
            Atom::Int(v) => Value::Int(*v),
            Atom::UInt(v) => Value::UInt(*v),
            Atom::Float(v) => Value::Float(*v),
            Atom::String(v) => Value::String(v.clone()),
            Atom::Bytes(v) => Value::Bytes(v.clone()),
            Atom::Bool(v) => Value::Bool(*v),
            Atom::Null => Value::Null,
        }
    }
}

impl ops::Add<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l + r).into(),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l + r).into(),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l + r).into(),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 + r).into(),
            (Value::Float(l), Value::Int(r)) => Value::Float(l + r as f64).into(),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 + r).into(),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l + r as f64).into(),

            (Value::List(l), Value::List(r)) => {
                Value::List(l.iter().chain(r.iter()).cloned().collect::<Vec<_>>().into()).into()
            }
            (Value::String(l), Value::String(r)) => {
                let mut new = String::with_capacity(l.len() + r.len());
                new.push_str(&l);
                new.push_str(&r);
                Value::String(new.into()).into()
            }
            // Merge two maps should overwrite keys in the left map with the right map
            (Value::Map(l), Value::Map(r)) => {
                let mut new = HashMap::default();
                for (k, v) in l.map.iter() {
                    new.insert(k.clone(), v.clone());
                }
                for (k, v) in r.map.iter() {
                    new.insert(k.clone(), v.clone());
                }
                Value::Map(Map { map: Arc::new(new) }).into()
            }
            (Value::Duration(l), Value::Duration(r)) => Value::Duration(l + r).into(),
            (Value::Timestamp(l), Value::Duration(r)) => Value::Timestamp(l + r).into(),
            (Value::Duration(l), Value::Timestamp(r)) => Value::Timestamp(r + l).into(),
            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "add", left, right,
            )),
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn sub(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l - r).into(),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l - r).into(),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l - r).into(),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 - r).into(),
            (Value::Float(l), Value::Int(r)) => Value::Float(l - r as f64).into(),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 - r).into(),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l - r as f64).into(),
            // todo: implement checked sub for these over-flowable operations
            (Value::Duration(l), Value::Duration(r)) => Value::Duration(l - r).into(),
            (Value::Timestamp(l), Value::Duration(r)) => Value::Timestamp(l - r).into(),
            (Value::Timestamp(l), Value::Timestamp(r)) => Value::Duration(l - r).into(),
            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "sub", left, right,
            )),
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn div(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l / r).into(),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l / r).into(),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l / r).into(),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 / r).into(),
            (Value::Float(l), Value::Int(r)) => Value::Float(l / r as f64).into(),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 / r).into(),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l / r as f64).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "div", left, right,
            )),
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn mul(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l * r).into(),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l * r).into(),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l * r).into(),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 * r).into(),
            (Value::Float(l), Value::Int(r)) => Value::Float(l * r as f64).into(),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 * r).into(),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l * r as f64).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "mul", left, right,
            )),
        }
    }
}

impl ops::Rem<Value> for Value {
    type Output = ResolveResult;

    #[inline(always)]
    fn rem(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l % r).into(),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l % r).into(),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l % r).into(),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 % r).into(),
            (Value::Float(l), Value::Int(r)) => Value::Float(l % r as f64).into(),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 % r).into(),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l % r as f64).into(),

            (left, right) => Err(ExecutionError::UnsupportedBinaryOperator(
                "rem", left, right,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{objects::Key, Context, ExecutionError, Program, Value};
    use std::collections::HashMap;
    use std::sync::Arc;

    #[test]
    fn test_indexed_map_access() {
        let mut context = Context::default();
        let mut headers = HashMap::new();
        headers.insert("Content-Type", "application/json".to_string());
        context.add_variable_from_value("headers", headers);

        let program = Program::compile("headers[\"Content-Type\"]").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, "application/json".into());
    }

    #[test]
    fn test_numeric_map_access() {
        let mut context = Context::default();
        let mut numbers = HashMap::new();
        numbers.insert(Key::Uint(1), "one".to_string());
        context.add_variable_from_value("numbers", numbers);

        let program = Program::compile("numbers[1]").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, "one".into());
    }

    #[test]
    fn test_heterogeneous_compare() {
        let context = Context::default();

        let program = Program::compile("1 < uint(2)").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());

        let program = Program::compile("1 < 1.1").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());

        let program = Program::compile("uint(0) > -10").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(
            value,
            true.into(),
            "negative signed ints should be less than uints"
        );
    }

    #[test]
    fn test_float_compare() {
        let context = Context::default();

        let program = Program::compile("1.0 > 0.0").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());

        let program = Program::compile("double('NaN') == double('NaN')").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, false.into(), "NaN should not equal itself");

        let program = Program::compile("1.0 > double('NaN')").unwrap();
        let result = program.execute(&context);
        assert!(
            result.is_err(),
            "NaN should not be comparable with inequality operators"
        );
    }

    #[test]
    fn test_invalid_compare() {
        let context = Context::default();

        let program = Program::compile("{} == []").unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, false.into());
    }

    #[test]
    fn test_size_fn_var() {
        let program = Program::compile("size(requests) + size == 5").unwrap();
        let mut context = Context::default();
        let requests = vec![Value::Int(42), Value::Int(42)];
        context
            .add_variable("requests", Value::List(Arc::new(requests)))
            .unwrap();
        context.add_variable("size", Value::Int(3)).unwrap();
        assert_eq!(program.execute(&context).unwrap(), Value::Bool(true));
    }

    fn test_execution_error(program: &str, expected: ExecutionError) {
        let program = Program::compile(program).unwrap();
        let result = program.execute(&Context::default());
        assert_eq!(result.unwrap_err(), expected);
    }

    #[test]
    fn test_invalid_sub() {
        test_execution_error(
            "'foo' - 10",
            ExecutionError::UnsupportedBinaryOperator("sub", "foo".into(), Value::Int(10)),
        );
    }

    #[test]
    fn test_invalid_add() {
        test_execution_error(
            "'foo' + 10",
            ExecutionError::UnsupportedBinaryOperator("add", "foo".into(), Value::Int(10)),
        );
    }

    #[test]
    fn test_invalid_div() {
        test_execution_error(
            "'foo' / 10",
            ExecutionError::UnsupportedBinaryOperator("div", "foo".into(), Value::Int(10)),
        );
    }

    #[test]
    fn test_invalid_rem() {
        test_execution_error(
            "'foo' % 10",
            ExecutionError::UnsupportedBinaryOperator("rem", "foo".into(), Value::Int(10)),
        );
    }

    #[test]
    fn out_of_bound_list_access() {
        let program = Program::compile("list[10]").unwrap();
        let mut context = Context::default();
        context
            .add_variable("list", Value::List(Arc::new(vec![])))
            .unwrap();
        let result = program.execute(&context);
        assert_eq!(result.unwrap(), Value::Null);
    }
}
