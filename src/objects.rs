use crate::ast::{ArithmeticOp, Atom, Expression, Member, UnaryOp};
use core::ops;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;
use crate::context::Context;

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum CelKey {
    Int(i32),
    Uint(u32),
    Bool(bool),
    String(Rc<str>),
}

impl<'a> TryInto<CelKey> for CelType<'a> {
    type Error = ();

    fn try_into(self) -> Result<CelKey, Self::Error> {
        match self {
            CelType::Int(v) => Ok(CelKey::Int(v)),
            CelType::UInt(v) => Ok(CelKey::Uint(v)),
            CelType::String(v) => Ok(CelKey::String(v)),
            CelType::Bool(v) => Ok(CelKey::Bool(v)),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CelType<'a> {
    List(Rc<[CelType<'a>]>),
    Map(Rc<HashMap<CelKey, CelType<'a>>>),

    Function(Rc<str>, Option<Box<CelType<'a>>>),

    // Atoms
    Int(i32),
    UInt(u32),
    Float(f64),
    String(Rc<str>),
    Bytes(&'a [u8]),
    Bool(bool),
    Null,
}

impl<'a> CelType<'a> {
    pub fn resolve(expr: &'a Expression<'a>, ctx: &'a Context<'a>) -> CelType<'a> {
        match expr {
            Expression::Atom(atom) => atom.into(),
            Expression::Arithmetic(left, op, right) => {
                let left = CelType::resolve(left, ctx);
                let right = CelType::resolve(right, ctx);

                match op {
                    ArithmeticOp::Add => left + right,
                    ArithmeticOp::Subtract => unimplemented!(),
                    ArithmeticOp::Divide => unimplemented!(),
                    ArithmeticOp::Multiply => unimplemented!(),
                    ArithmeticOp::Modulus => unimplemented!(),
                }
            }
            Expression::Relation(_, _, _) => unimplemented!(),
            Expression::Ternary(cond, left, right) => {
                let cond = CelType::resolve(cond, ctx);
                if cond.to_bool() {
                    CelType::resolve(left, ctx)
                } else {
                    CelType::resolve(right, ctx)
                }
            }
            Expression::Or(left, right) => {
                let left = CelType::resolve(left, ctx);
                if left.to_bool() {
                    left
                } else {
                    CelType::resolve(right, ctx)
                }
            }
            Expression::And(left, right) => {
                let left = CelType::resolve(left, ctx);
                let right = CelType::resolve(right, ctx);
                CelType::Bool(left.to_bool() && right.to_bool())
            }
            Expression::Unary(op, expr) => {
                let expr = CelType::resolve(expr, ctx);
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
            }
            Expression::Member(left, right) => {
                let left = CelType::resolve(left, ctx);
                left.member(right, ctx)
            }
            Expression::List(items) => {
                let list = items.iter().map(|i| CelType::resolve(i, ctx)).collect();
                CelType::List(list)
            }
            Expression::Map(items) => {
                let map: HashMap<CelKey, CelType> = items
                    .iter()
                    .map(|(k, v)| {
                        let key = CelType::resolve(k, ctx).try_into().unwrap();
                        let value = CelType::resolve(v, ctx);
                        (key, value)
                    })
                    .collect();
                CelType::Map(Rc::from(map))
            }
            Expression::Ident(name) => {
                if ctx.functions.contains_key(name) {
                    CelType::Function((*name).into(), None)
                } else {
                    unreachable!();
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

    fn member(self, member: &'a Member<'a>, ctx: &'a Context<'a>) -> CelType<'a> {
        match member {
            Member::Index(idx) => {
                let idx = CelType::resolve(idx, ctx);
                match (self, idx) {
                    (CelType::List(items), CelType::Int(idx)) => {
                        items.get(idx as usize).unwrap().clone()
                    }
                    _ => unimplemented!(),
                }
            }
            Member::Fields(_) => unimplemented!(),
            Member::Attribute(name) => {
                if ctx.functions.contains_key(name) {
                    CelType::Function((*name).into(), Some(self.into()))
                } else {
                    unreachable!();
                }
            },
            Member::FunctionCall(args) => {
                if let CelType::Function(name, target) = self {
                    let func = ctx.functions.get(name.as_ref()).unwrap();
                    let target = match target {
                        None => None,
                        Some(t) => Some(*t)
                    };
                    return func(target.as_ref(), args, ctx)
                } else {
                    unreachable!("FunctionCall without CelType::Function - {:?}", self)
                }
            },
        }
    }

    fn to_bool(&self) -> bool {
        match self {
            CelType::List(v) => !v.is_empty(),
            CelType::Map(v) => !v.is_empty(),
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

impl<'a> From<&'a Atom<'a>> for CelType<'a> {
    fn from(atom: &'a Atom<'a>) -> Self {
        match atom {
            Atom::Int(v) => CelType::Int(*v),
            Atom::UInt(v) => CelType::UInt(*v),
            Atom::Float(v) => CelType::Float(*v),
            Atom::String(v) => CelType::String(v.to_string().into()),
            Atom::Bytes(v) => CelType::Bytes(v),
            Atom::Bool(v) => CelType::Bool(*v),
            Atom::Null => CelType::Null,
        }
    }
}

impl<'a> ops::Add<CelType<'a>> for CelType<'a> {
    type Output = CelType<'a>;

    fn add(self, rhs: CelType<'a>) -> Self::Output {
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
                let new = l.iter().chain(r.iter()).cloned().collect();

                CelType::List(new)
            }
            (CelType::String(l), CelType::String(r)) => {
                let mut new = String::with_capacity(l.len() + r.len());
                new.push_str(&l);
                new.push_str(&r);
                CelType::String(new.into())
            }
            _ => unimplemented!(),
        }
    }
}
