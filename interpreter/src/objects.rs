use crate::context::Context;
use crate::ExecutionError;
use crate::ExecutionError::NoSuchKey;
use cel_parser::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};
use core::ops;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

// Easily create conversions from primitive types to CelType
macro_rules! impl_from {
    ($($t:ty => $v:expr),*) => {
        $(
            impl From<$t> for CelType {
                fn from(v: $t) -> Self {
                    $v(v)
                }
            }
        )*
    };
}

#[derive(Debug, PartialEq, Clone)]
pub struct CelMap {
    pub map: Rc<HashMap<CelKey, CelType>>,
}

impl PartialOrd for CelMap {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        None
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Ord, Clone, PartialOrd)]
pub enum CelKey {
    Int(i32),
    Uint(u32),
    Bool(bool),
    String(Rc<String>),
}

/// Implement conversions from primitive types to [`CelKey`]

impl From<String> for CelKey {
    fn from(v: String) -> Self {
        CelKey::String(v.into())
    }
}

impl From<Rc<String>> for CelKey {
    fn from(v: Rc<String>) -> Self {
        CelKey::String(v.clone())
    }
}

impl<'a> From<&'a str> for CelKey {
    fn from(v: &'a str) -> Self {
        CelKey::String(Rc::new(v.into()))
    }
}

impl From<bool> for CelKey {
    fn from(v: bool) -> Self {
        CelKey::Bool(v)
    }
}

impl From<i32> for CelKey {
    fn from(v: i32) -> Self {
        CelKey::Int(v)
    }
}

impl From<u32> for CelKey {
    fn from(v: u32) -> Self {
        CelKey::Uint(v)
    }
}

/// Implement conversions from [`CelKey`] into [`CelType`]

impl TryInto<CelKey> for CelType {
    type Error = CelType;

    #[inline(always)]
    fn try_into(self) -> Result<CelKey, Self::Error> {
        match self {
            CelType::Int(v) => Ok(CelKey::Int(v)),
            CelType::UInt(v) => Ok(CelKey::Uint(v)),
            CelType::String(v) => Ok(CelKey::String(v)),
            CelType::Bool(v) => Ok(CelKey::Bool(v)),
            _ => Err(self),
        }
    }
}

// Implement conversion from HashMap<K, V> into CelMap
impl<K: Into<CelKey>, V: Into<CelType>> From<HashMap<K, V>> for CelMap {
    fn from(map: HashMap<K, V>) -> Self {
        let mut new_map = HashMap::new();
        for (k, v) in map {
            new_map.insert(k.into(), v.into());
        }
        CelMap {
            map: Rc::new(new_map),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CelType {
    List(Rc<Vec<CelType>>),
    Map(CelMap),

    Function(Rc<String>, Option<Box<CelType>>),

    // Atoms
    Int(i32),
    UInt(u32),
    Float(f64),
    String(Rc<String>),
    Bytes(Rc<Vec<u8>>),
    Bool(bool),
    Null,
}

impl Eq for CelType {}

impl PartialOrd for CelType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CelType {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (CelType::Int(a), CelType::Int(b)) => a.cmp(b),
            (CelType::UInt(a), CelType::UInt(b)) => a.cmp(b),
            (CelType::Float(a), CelType::Float(b)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
            (CelType::String(a), CelType::String(b)) => a.cmp(b),
            (CelType::Bool(a), CelType::Bool(b)) => a.cmp(b),
            (CelType::Null, CelType::Null) => Ordering::Equal,
            _ => unreachable!(),
        }
    }
}

impl From<&CelKey> for CelType {
    fn from(value: &CelKey) -> Self {
        match value {
            CelKey::Int(v) => CelType::Int(*v),
            CelKey::Uint(v) => CelType::UInt(*v),
            CelKey::Bool(v) => CelType::Bool(*v),
            CelKey::String(v) => CelType::String(v.clone()),
        }
    }
}

impl From<CelKey> for CelType {
    fn from(value: CelKey) -> Self {
        match value {
            CelKey::Int(v) => CelType::Int(v),
            CelKey::Uint(v) => CelType::UInt(v),
            CelKey::Bool(v) => CelType::Bool(v),
            CelKey::String(v) => CelType::String(v),
        }
    }
}

// Convert Vec<T> to CelType
impl<T: Into<CelType>> From<Vec<T>> for CelType {
    fn from(v: Vec<T>) -> Self {
        CelType::List(v.into_iter().map(|v| v.into()).collect::<Vec<_>>().into())
    }
}

// Convert Vec<u8> to CelType
impl From<Vec<u8>> for CelType {
    fn from(v: Vec<u8>) -> Self {
        CelType::Bytes(v.into())
    }
}

// Convert String to CelType
impl From<String> for CelType {
    fn from(v: String) -> Self {
        CelType::String(v.into())
    }
}

// Convert Option<T> to CelType
impl<T: Into<CelType>> From<Option<T>> for CelType {
    fn from(v: Option<T>) -> Self {
        match v {
            Some(v) => v.into(),
            None => CelType::Null,
        }
    }
}

// Convert HashMap<K, V> to CelType
impl<K: Into<CelKey>, V: Into<CelType>> From<HashMap<K, V>> for CelType {
    fn from(v: HashMap<K, V>) -> Self {
        CelType::Map(v.into())
    }
}

impl_from!(
    i32 => CelType::Int,
    f64 => CelType::Float,
    bool => CelType::Bool
);

impl From<ExecutionError> for ResolveResult {
    fn from(value: ExecutionError) -> Self {
        Err(value)
    }
}

pub type ResolveResult = Result<CelType, ExecutionError>;

impl From<CelType> for ResolveResult {
    fn from(value: CelType) -> Self {
        Ok(value)
    }
}

impl<'a> CelType {
    pub fn resolve_all(expr: &'a [Expression], ctx: &Context) -> ResolveResult {
        let mut res = Vec::with_capacity(expr.len());
        for expr in expr {
            res.push(CelType::resolve(expr, ctx)?);
        }
        Ok(CelType::List(res.into()))
    }

    #[inline(always)]
    pub fn resolve(expr: &'a Expression, ctx: &Context) -> ResolveResult {
        match expr {
            Expression::Atom(atom) => Ok(atom.into()),
            Expression::Arithmetic(left, op, right) => {
                let left = CelType::resolve(left, ctx)?;
                let right = CelType::resolve(right, ctx)?;

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
                let left = CelType::resolve(left, ctx)?;
                let right = CelType::resolve(right, ctx)?;
                let res = match op {
                    RelationOp::LessThan => left < right,
                    RelationOp::LessThanEq => left <= right,
                    RelationOp::GreaterThan => left > right,
                    RelationOp::GreaterThanEq => left >= right,
                    RelationOp::Equals => right.eq(&left),
                    RelationOp::NotEquals => right.ne(&left),
                    RelationOp::In => match (left, right) {
                        (CelType::String(l), CelType::String(r)) => r.contains(&*l),
                        (any, CelType::List(v)) => v.contains(&any),
                        (any, CelType::Map(m)) => m.map.contains_key(&any.try_into().unwrap()),
                        _ => unimplemented!(),
                    },
                };
                CelType::Bool(res).into()
            }
            Expression::Ternary(cond, left, right) => {
                let cond = CelType::resolve(cond, ctx)?;
                if cond.to_bool() {
                    CelType::resolve(left, ctx)
                } else {
                    CelType::resolve(right, ctx)
                }
            }
            Expression::Or(left, right) => {
                let left = CelType::resolve(left, ctx)?;
                if left.to_bool() {
                    left.into()
                } else {
                    CelType::resolve(right, ctx)
                }
            }
            Expression::And(left, right) => {
                let left = CelType::resolve(left, ctx)?;
                let right = CelType::resolve(right, ctx)?;
                CelType::Bool(left.to_bool() && right.to_bool()).into()
            }
            Expression::Unary(op, expr) => {
                let expr = CelType::resolve(expr, ctx)?;
                match op {
                    UnaryOp::Not => CelType::Bool(!expr.to_bool()),
                    UnaryOp::DoubleNot => CelType::Bool(expr.to_bool()),
                    UnaryOp::Minus => match expr {
                        CelType::Int(i) => CelType::Int(-i),
                        CelType::Float(i) => CelType::Float(-i),
                        _ => unimplemented!(),
                    },
                    UnaryOp::DoubleMinus => match expr {
                        CelType::Int(_) => expr,
                        CelType::UInt(_) => expr,
                        CelType::Float(_) => expr,
                        _ => unimplemented!(),
                    },
                }
                .into()
            }
            Expression::Member(left, right) => {
                let left = CelType::resolve(left, ctx)?;
                left.member(right, ctx)
            }
            Expression::List(items) => {
                let list = items
                    .iter()
                    .map(|i| CelType::resolve(i, ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                CelType::List(list.into()).into()
            }
            Expression::Map(items) => {
                let mut map = HashMap::default();
                for (k, v) in items.iter() {
                    let key = CelType::resolve(k, ctx)?
                        .try_into()
                        .map_err(ExecutionError::UnsupportedKeyType)?;
                    let value = CelType::resolve(v, ctx)?;
                    map.insert(key, value);
                }
                CelType::Map(CelMap { map: Rc::from(map) }).into()
            }
            Expression::Ident(name) => {
                if ctx.has_function(&***name) {
                    CelType::Function(name.clone(), None).into()
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
                let idx = CelType::resolve(idx, ctx)?;
                match (self, idx) {
                    (CelType::List(items), CelType::Int(idx)) => {
                        items.get(idx as usize).unwrap().clone().into()
                    }
                    (CelType::String(str), CelType::Int(idx)) => {
                        match str.get(idx as usize..(idx + 1) as usize) {
                            None => CelType::Null,
                            Some(str) => CelType::String(str.to_string().into()),
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
                    CelType::Map(ref m) => m.map.get(&name.clone().into()).cloned(),
                    _ => None,
                };

                // If the property is both an attribute and a method, then we
                // give priority to the property. Maybe we can implement lookahead
                // to see if the next token is a function call?
                match (child.is_some(), ctx.has_function(&***name)) {
                    (false, false) => NoSuchKey(name.clone()).into(),
                    (true, true) | (true, false) => child.unwrap().into(),
                    (false, true) => CelType::Function(name.clone(), Some(self.into())).into(),
                }
            }
            Member::FunctionCall(args) => {
                if let CelType::Function(name, target) = self {
                    let func = ctx.get_function(&**name).unwrap();
                    match target {
                        None => {
                            if args.is_empty() {
                                return Err(ExecutionError::MissingArgumentOrTarget);
                            }
                            func(None, args, ctx)
                        }
                        Some(t) => func(Some(t.as_ref()), args, ctx),
                    }
                } else {
                    unreachable!("FunctionCall without CelType::Function - {:?}", self)
                }
            }
        }
    }

    #[inline(always)]
    fn to_bool(&self) -> bool {
        match self {
            CelType::List(v) => !v.is_empty(),
            CelType::Map(v) => !v.map.is_empty(),
            CelType::Int(v) => *v != 0,
            CelType::UInt(v) => *v != 0,
            CelType::Float(v) => *v != 0.0,
            CelType::String(v) => !v.is_empty(),
            CelType::Bytes(v) => !v.is_empty(),
            CelType::Bool(v) => *v,
            CelType::Null => false,
            CelType::Function(_, _) => false,
        }
    }
}

impl From<&Atom> for CelType {
    #[inline(always)]
    fn from(atom: &Atom) -> Self {
        match atom {
            Atom::Int(v) => CelType::Int(*v),
            Atom::UInt(v) => CelType::UInt(*v),
            Atom::Float(v) => CelType::Float(*v),
            Atom::String(v) => CelType::String(v.clone()),
            Atom::Bytes(v) => CelType::Bytes(v.clone()),
            Atom::Bool(v) => CelType::Bool(*v),
            Atom::Null => CelType::Null,
        }
    }
}

impl ops::Add<CelType> for CelType {
    type Output = CelType;

    #[inline(always)]
    fn add(self, rhs: CelType) -> Self::Output {
        match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l + r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l + r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l + r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 + r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l + r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 + r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l + r as f64),

            (CelType::List(l), CelType::List(r)) => {
                CelType::List(l.iter().chain(r.iter()).cloned().collect::<Vec<_>>().into())
            }
            (CelType::String(l), CelType::String(r)) => {
                let mut new = String::with_capacity(l.len() + r.len());
                new.push_str(&l);
                new.push_str(&r);
                CelType::String(new.into())
            }
            // Merge two maps should overwrite keys in the left map with the right map
            (CelType::Map(l), CelType::Map(r)) => {
                let mut new = HashMap::default();
                for (k, v) in l.map.iter() {
                    new.insert(k.clone(), v.clone());
                }
                for (k, v) in r.map.iter() {
                    new.insert(k.clone(), v.clone());
                }
                CelType::Map(CelMap { map: Rc::new(new) })
            }
            _ => unimplemented!(),
        }
    }
}

impl ops::Sub<CelType> for CelType {
    type Output = CelType;

    #[inline(always)]
    fn sub(self, rhs: CelType) -> Self::Output {
        match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l - r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l - r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l - r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 - r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l - r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 - r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l - r as f64),
            _ => unimplemented!(),
        }
    }
}

impl ops::Div<CelType> for CelType {
    type Output = CelType;

    #[inline(always)]
    fn div(self, rhs: CelType) -> Self::Output {
        match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l / r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l / r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l / r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 / r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l / r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 / r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l / r as f64),

            _ => unimplemented!(),
        }
    }
}

impl ops::Mul<CelType> for CelType {
    type Output = CelType;

    #[inline(always)]
    fn mul(self, rhs: CelType) -> Self::Output {
        match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l * r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l * r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l * r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 * r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l * r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 * r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l * r as f64),

            _ => unimplemented!(),
        }
    }
}

impl ops::Rem<CelType> for CelType {
    type Output = CelType;

    #[inline(always)]
    fn rem(self, rhs: CelType) -> Self::Output {
        match (self, rhs) {
            (CelType::Int(l), CelType::Int(r)) => CelType::Int(l % r),
            (CelType::UInt(l), CelType::UInt(r)) => CelType::UInt(l % r),

            // Float matrix
            (CelType::Float(l), CelType::Float(r)) => CelType::Float(l % r),
            (CelType::Int(l), CelType::Float(r)) => CelType::Float(l as f64 % r),
            (CelType::Float(l), CelType::Int(r)) => CelType::Float(l % r as f64),
            (CelType::UInt(l), CelType::Float(r)) => CelType::Float(l as f64 % r),
            (CelType::Float(l), CelType::UInt(r)) => CelType::Float(l % r as f64),

            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::objects::CelType;

    #[test]
    fn test_from_primitives() {
        let value: CelType = 1i32.into();
        assert_eq!(value, CelType::Int(1));
    }
}
