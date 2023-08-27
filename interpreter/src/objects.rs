use crate::context::Context;
use crate::functions::FunctionContext;
use crate::ExecutionError;
use crate::ExecutionError::NoSuchKey;
use cel_parser::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};
use chrono::{DateTime, Duration, FixedOffset};
use core::ops;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Map {
    pub map: Rc<HashMap<Key, Value>>,
}

impl PartialOrd for Map {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        None
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Ord, Clone, PartialOrd)]
pub enum Key {
    Int(i64),
    Uint(u64),
    Bool(bool),
    String(Rc<String>),
}

/// Implement conversions from primitive types to [`Key`]

impl From<String> for Key {
    fn from(v: String) -> Self {
        Key::String(v.into())
    }
}

impl From<Rc<String>> for Key {
    fn from(v: Rc<String>) -> Self {
        Key::String(v.clone())
    }
}

impl<'a> From<&'a str> for Key {
    fn from(v: &'a str) -> Self {
        Key::String(Rc::new(v.into()))
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
            map: Rc::new(new_map),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    List(Rc<Vec<Value>>),
    Map(Map),

    Function(Rc<String>, Option<Box<Value>>),

    // Atoms
    Int(i64),
    UInt(u64),
    Float(f64),
    String(Rc<String>),
    Bytes(Rc<Vec<u8>>),
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

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::UInt(a), Value::UInt(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
            (Value::String(a), Value::String(b)) => a.cmp(b),
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Null, Value::Null) => Ordering::Equal,
            (Value::Duration(a), Value::Duration(b)) => a.cmp(b),
            (Value::Timestamp(a), Value::Timestamp(b)) => a.cmp(b),
            (a, b) => panic!("unable to compare {:?} with {:?}", a, b),
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
                .into()
            }
            Expression::Relation(left, op, right) => {
                let left = Value::resolve(left, ctx)?;
                let right = Value::resolve(right, ctx)?;
                let res = match op {
                    RelationOp::LessThan => left < right,
                    RelationOp::LessThanEq => left <= right,
                    RelationOp::GreaterThan => left > right,
                    RelationOp::GreaterThanEq => left >= right,
                    RelationOp::Equals => right.eq(&left),
                    RelationOp::NotEquals => right.ne(&left),
                    RelationOp::In => match (left, right) {
                        (Value::String(l), Value::String(r)) => r.contains(&*l),
                        (any, Value::List(v)) => v.contains(&any),
                        (any, Value::Map(m)) => m.map.contains_key(&any.try_into().unwrap()),
                        _ => unimplemented!(),
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
                    UnaryOp::Not => Value::Bool(!expr.to_bool()),
                    UnaryOp::DoubleNot => Value::Bool(expr.to_bool()),
                    UnaryOp::Minus => match expr {
                        Value::Int(i) => Value::Int(-i),
                        Value::Float(i) => Value::Float(-i),
                        _ => unimplemented!(),
                    },
                    UnaryOp::DoubleMinus => match expr {
                        Value::Int(_) => expr,
                        Value::UInt(_) => expr,
                        Value::Float(_) => expr,
                        _ => unimplemented!(),
                    },
                }
                .into()
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
                Value::Map(Map { map: Rc::from(map) }).into()
            }
            Expression::Ident(name) => {
                if ctx.has_function(&***name) {
                    Value::Function(name.clone(), None).into()
                } else {
                    ctx.get_variable(&***name)
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
                    (Value::List(items), Value::Int(idx)) => {
                        items.get(idx as usize).unwrap().clone().into()
                    }
                    (Value::String(str), Value::Int(idx)) => {
                        match str.get(idx as usize..(idx + 1) as usize) {
                            None => Value::Null,
                            Some(str) => Value::String(str.to_string().into()),
                        }
                        .into()
                    }
                    _ => unimplemented!(),
                }
            }
            Member::Fields(_) => unimplemented!(),
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
                match (child.is_some(), ctx.has_function(&***name)) {
                    (false, false) => NoSuchKey(name.clone()).into(),
                    (true, true) | (true, false) => child.unwrap().into(),
                    (false, true) => Value::Function(name.clone(), Some(self.into())).into(),
                }
            }
            Member::FunctionCall(args) => {
                if let Value::Function(name, target) = self {
                    let func = ctx.get_function(&**name).unwrap();
                    match target {
                        None => {
                            let mut ctx =
                                FunctionContext::new(name.clone(), None, ctx, args.clone());
                            func.call_with_context(&mut ctx)
                        }
                        Some(target) => {
                            let mut ctx = FunctionContext::new(
                                name.clone(),
                                Some(*target),
                                ctx,
                                args.clone(),
                            );
                            func.call_with_context(&mut ctx)
                        }
                    }
                } else {
                    unreachable!("FunctionCall without Value::Function - {:?}", self)
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
            Value::Timestamp(v) => v.timestamp_nanos() > 0,
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
    type Output = Value;

    #[inline(always)]
    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l + r),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 + r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l + r as f64),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 + r),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l + r as f64),

            (Value::List(l), Value::List(r)) => {
                Value::List(l.iter().chain(r.iter()).cloned().collect::<Vec<_>>().into())
            }
            (Value::String(l), Value::String(r)) => {
                let mut new = String::with_capacity(l.len() + r.len());
                new.push_str(&l);
                new.push_str(&r);
                Value::String(new.into())
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
                Value::Map(Map { map: Rc::new(new) })
            }
            (Value::Duration(l), Value::Duration(r)) => Value::Duration(l + r),
            (Value::Timestamp(l), Value::Duration(r)) => Value::Timestamp(l + r),
            (Value::Duration(l), Value::Timestamp(r)) => Value::Timestamp(r + l),
            _ => unimplemented!(),
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;

    #[inline(always)]
    fn sub(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l - r),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 - r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l - r as f64),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 - r),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l - r as f64),
            // todo: implement checked sub for these over-flowable operations
            (Value::Duration(l), Value::Duration(r)) => Value::Duration(l - r),
            (Value::Timestamp(l), Value::Duration(r)) => Value::Timestamp(l - r),
            (Value::Timestamp(l), Value::Timestamp(r)) => Value::Duration(l - r),
            _ => unimplemented!(),
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = Value;

    #[inline(always)]
    fn div(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l / r),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 / r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l / r as f64),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 / r),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l / r as f64),

            _ => unimplemented!(),
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = Value;

    #[inline(always)]
    fn mul(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l * r),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 * r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l * r as f64),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 * r),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l * r as f64),

            _ => unimplemented!(),
        }
    }
}

impl ops::Rem<Value> for Value {
    type Output = Value;

    #[inline(always)]
    fn rem(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l % r),
            (Value::UInt(l), Value::UInt(r)) => Value::UInt(l % r),

            // Float matrix
            (Value::Float(l), Value::Float(r)) => Value::Float(l % r),
            (Value::Int(l), Value::Float(r)) => Value::Float(l as f64 % r),
            (Value::Float(l), Value::Int(r)) => Value::Float(l % r as f64),
            (Value::UInt(l), Value::Float(r)) => Value::Float(l as f64 % r),
            (Value::Float(l), Value::UInt(r)) => Value::Float(l % r as f64),

            _ => unimplemented!(),
        }
    }
}
