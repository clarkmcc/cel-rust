use std::sync::Arc;

use cel_parser::{ArithmeticOp, Atom, Expression, Member, RelationOp, UnaryOp};

use constant::ConstantKind;
use expr::{
    create_struct::{entry::KeyKind, Entry},
    Call, CreateList, CreateStruct, ExprKind, Ident, Select,
};

include!(concat!(env!("OUT_DIR"), "/cel.expr.rs"));

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("ExpressionNotSet")]
    ExpressionNotSet,

    #[error("Unimplemented: {0}")]
    Unimplemented(String),
}

impl TryFrom<Expr> for Expression {
    type Error = Error;

    fn try_from(proto_expr: Expr) -> Result<Self, Self::Error> {
        let kind = proto_expr.expr_kind.ok_or(Error::ExpressionNotSet)?;

        match kind {
            ExprKind::ConstExpr(c) => {
                match c.constant_kind.ok_or(Error::ExpressionNotSet)? {
                    ConstantKind::NullValue(_) => Ok(Expression::Atom(Atom::Null)),
                    ConstantKind::BoolValue(b) => Ok(Expression::Atom(Atom::Bool(b))),
                    ConstantKind::Int64Value(n) => Ok(Expression::Atom(Atom::Int(n))),
                    ConstantKind::Uint64Value(n) => Ok(Expression::Atom(Atom::UInt(n))),
                    ConstantKind::DoubleValue(n) => Ok(Expression::Atom(Atom::Float(n))),
                    ConstantKind::StringValue(s) => Ok(Expression::Atom(Atom::String(Arc::new(s)))),
                    ConstantKind::BytesValue(b) => Ok(Expression::Atom(Atom::Bytes(Arc::new(b)))),
                    ConstantKind::DurationValue(_d) => {
                        // Ok(Value::Duration(TimeDelta::new(d.seconds, d.nanos as u32)))
                        Err(Error::Unimplemented("Duration constant".to_string()))
                    }
                    ConstantKind::TimestampValue(_t) => {
                        // Ok(crate::objects::Value::Timestamp(
                        //     DateTime::from_timestamp(t.seconds, t.nanos).unwrap_or_default(),
                        // ))
                        Err(Error::Unimplemented("Timestamp constant".to_string()))
                    }
                }
            }
            ExprKind::IdentExpr(i) => Ok(Expression::Ident(Arc::new(i.name))),
            ExprKind::SelectExpr(s) => {
                let operand_expr =
                    Expression::try_from(*s.operand.ok_or(Error::ExpressionNotSet)?)?;

                Ok(Expression::Member(
                    Box::new(operand_expr),
                    Box::new(Member::Attribute(Arc::new(s.field))),
                ))
            }
            ExprKind::CallExpr(c) => {
                let func = Box::new(Expression::Ident(Arc::new(c.function)));

                let target = c
                    .target
                    .map(|t| Expression::try_from(*t))
                    .transpose()?
                    .map(Box::new);

                let args = c
                    .args
                    .into_iter()
                    .map(Expression::try_from)
                    .collect::<Result<_, _>>()?;

                Ok(Expression::FunctionCall(func, target, args))
            }
            ExprKind::ListExpr(l) => {
                let elements = l
                    .elements
                    .into_iter()
                    .map(Expression::try_from)
                    .collect::<Result<_, _>>()?;

                Ok(Expression::List(elements))
            }
            ExprKind::StructExpr(s) => {
                let fields = s
                    .entries
                    .into_iter()
                    .map(|entry| {
                        let key_expr = match entry.key_kind {
                            Some(KeyKind::MapKey(e)) => Ok(e),
                            Some(KeyKind::FieldKey(_s)) => {
                                Err(Error::Unimplemented("Field key in struct expr".to_string()))
                            }
                            None => Err(Error::ExpressionNotSet),
                        }?;
                        let value_expr = entry.value.ok_or(Error::ExpressionNotSet)?;

                        let key = Expression::try_from(key_expr)?;
                        let value = Expression::try_from(value_expr)?;

                        Ok((key, value))
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Expression::Map(fields))
            }
            ExprKind::ComprehensionExpr(_c) => {
                Err(Error::Unimplemented("Comprehension expr".to_string()))
            }
        }
    }
}

// From: https://github.com/google/cel-go/blob/6202a67201234710859d7b0e4c75a85a428fd520/common/operators/operators.go#L22-L41
// // Symbolic operators.
//
// Conditional   = "_?_:_"
// LogicalAnd    = "_&&_"
// LogicalOr     = "_||_"
// LogicalNot    = "!_"
// Equals        = "_==_"
// NotEquals     = "_!=_"
// Less          = "_<_"
// LessEquals    = "_<=_"
// Greater       = "_>_"
// GreaterEquals = "_>=_"
// Add           = "_+_"
// Subtract      = "_-_"
// Multiply      = "_*_"
// Divide        = "_/_"
// Modulo        = "_%_"
// Negate        = "-_"
// Index         = "_[_]"
// OptIndex      = "_[?_]"
// OptSelect     = "_?._"
//
// // Macros, must have a valid identifier.
// Has       = "has"
// All       = "all"
// Exists    = "exists"
// ExistsOne = "exists_one"
// Map       = "map"
// Filter    = "filter"
//
// // Named operators, must not have be valid identifiers.
// NotStrictlyFalse = "@not_strictly_false"
// In               = "@in"

fn binop_call_expr<S: Into<String>>(
    lhs: Expression,
    op: S,
    rhs: Expression,
) -> Result<ExprKind, Error> {
    Ok(ExprKind::CallExpr(Box::new(Call {
        target: Some(Box::new(lhs.try_into()?)),
        function: op.into(),
        args: vec![rhs.try_into()?],
    })))
}

impl TryFrom<Expression> for Expr {
    type Error = Error;

    fn try_from(expr: Expression) -> Result<Self, Self::Error> {
        match expr {
            Expression::Arithmetic(lhs, op, rhs) => {
                let call_expr = match op {
                    ArithmeticOp::Add => binop_call_expr(*lhs, "_+_", *rhs),
                    ArithmeticOp::Subtract => binop_call_expr(*lhs, "_-_", *rhs),
                    ArithmeticOp::Divide => binop_call_expr(*lhs, "_/_", *rhs),
                    ArithmeticOp::Multiply => binop_call_expr(*lhs, "_*_", *rhs),
                    ArithmeticOp::Modulus => binop_call_expr(*lhs, "_%_", *rhs),
                }?;

                Ok(Expr {
                    id: 0,
                    expr_kind: Some(call_expr),
                })
            }
            Expression::Relation(lhs, op, rhs) => {
                let call_expr = match op {
                    RelationOp::LessThan => binop_call_expr(*lhs, "_<_", *rhs),
                    RelationOp::LessThanEq => binop_call_expr(*lhs, "_<=_", *rhs),
                    RelationOp::GreaterThan => binop_call_expr(*lhs, "_>_", *rhs),
                    RelationOp::GreaterThanEq => binop_call_expr(*lhs, "_>=_", *rhs),
                    RelationOp::Equals => binop_call_expr(*lhs, "_==_", *rhs),
                    RelationOp::NotEquals => binop_call_expr(*lhs, "_!=_", *rhs),
                    RelationOp::In => binop_call_expr(*lhs, "@in", *rhs),
                }?;

                Ok(Expr {
                    id: 0,
                    expr_kind: Some(call_expr),
                })
            }
            Expression::Ternary(cond, left, right) => {
                let target = Some(Box::new(Expr::try_from(*cond)?));

                Ok(Expr {
                    id: 0,
                    expr_kind: Some(ExprKind::CallExpr(Box::new(Call {
                        target,
                        function: "_?_:_".to_string(),
                        args: vec![Expr::try_from(*left)?, Expr::try_from(*right)?],
                    }))),
                })
            }
            Expression::Or(lhs, rhs) => {
                let call_expr = binop_call_expr(*lhs, "_||_", *rhs)?;
                Ok(Expr {
                    id: 0,
                    expr_kind: Some(call_expr),
                })
            }
            Expression::And(lhs, rhs) => {
                let call_expr = binop_call_expr(*lhs, "_&&_", *rhs)?;
                Ok(Expr {
                    id: 0,
                    expr_kind: Some(call_expr),
                })
            }
            Expression::Unary(op, e) => {
                let func = match op {
                    UnaryOp::Not => Some("!_"),
                    UnaryOp::Minus => Some("-_"),
                    UnaryOp::DoubleNot | UnaryOp::DoubleMinus => None,
                };

                match func {
                    None => Expr::try_from(*e),
                    Some(name) => Ok(Expr {
                        id: 0,
                        expr_kind: Some(ExprKind::CallExpr(Box::new(Call {
                            target: None,
                            function: name.to_string(),
                            args: vec![Expr::try_from(*e)?],
                        }))),
                    }),
                }
            }
            Expression::Member(left, right) => match *right {
                Member::Attribute(field) => {
                    let operand = Some(Box::new(Expr::try_from(*left)?));

                    Ok(Expr {
                        id: 0,
                        expr_kind: Some(ExprKind::SelectExpr(Box::new(Select {
                            operand,
                            field: field.as_ref().clone(),
                            test_only: false, // TODO: has(parent.field) should become `test_only: true`
                        }))),
                    })
                }
                Member::Index(index) => {
                    let call_expr = binop_call_expr(*left, "_[_]", *index)?;

                    Ok(Expr {
                        id: 0,
                        expr_kind: Some(call_expr),
                    })
                }
                Member::Fields(_fields) => Err(Error::Unimplemented(
                    "Member Fields to proto conversion".to_string(),
                )),
            },
            Expression::FunctionCall(expr, target, args) => {
                let target = target
                    .map(|t| Expr::try_from(*t))
                    .transpose()?
                    .map(Box::new);

                let ident = match *expr {
                    Expression::Ident(ident) => Ok(ident.as_ref().clone()),
                    other => Err(Error::Unimplemented(format!(
                        "proto function call expects an identifier, got: {:?}",
                        other
                    ))),
                }?;

                let args = args
                    .into_iter()
                    .map(Expr::try_from)
                    .collect::<Result<_, _>>()?;

                Ok(Expr {
                    id: 0,
                    expr_kind: Some(ExprKind::CallExpr(Box::new(Call {
                        target,
                        function: ident,
                        args,
                    }))),
                })
            }
            Expression::List(exprs) => {
                let list_kind = ExprKind::ListExpr(CreateList {
                    elements: exprs
                        .into_iter()
                        .map(Expr::try_from)
                        .collect::<Result<_, _>>()?,
                    optional_indices: Vec::new(), // TODO: If list is dynamic or can contain None/null, this should be set
                });

                Ok(Expr {
                    id: 0,
                    expr_kind: Some(list_kind),
                })
            }
            Expression::Map(exprs) => {
                let struct_kind = ExprKind::StructExpr(CreateStruct {
                    entries: exprs
                        .into_iter()
                        .map(|(key, val)| {
                            let k = Expr::try_from(key)?;
                            let v = Expr::try_from(val)?;

                            Ok(Entry {
                                id: 0,
                                key_kind: Some(KeyKind::MapKey(k)),
                                value: Some(v),
                                optional_entry: false,
                            })
                        })
                        .collect::<Result<_, _>>()?,
                    message_name: String::new(),
                });

                Ok(Expr {
                    id: 0,
                    expr_kind: Some(struct_kind),
                })
            }
            Expression::Atom(atom) => {
                let const_ = match atom {
                    Atom::Int(n) => ConstantKind::Int64Value(n),
                    Atom::UInt(n) => ConstantKind::Uint64Value(n),
                    Atom::Float(n) => ConstantKind::DoubleValue(n),
                    Atom::String(s) => ConstantKind::StringValue(s.as_ref().clone()),
                    Atom::Bytes(b) => ConstantKind::BytesValue(b.as_ref().clone()),
                    Atom::Bool(b) => ConstantKind::BoolValue(b),
                    Atom::Null => ConstantKind::NullValue(0),
                };

                Ok(Expr {
                    id: 0,
                    expr_kind: Some(ExprKind::ConstExpr(Constant {
                        constant_kind: Some(const_),
                    })),
                })
            }
            Expression::Ident(s) => Ok(Expr {
                id: 0,
                expr_kind: Some(ExprKind::IdentExpr(Ident {
                    name: s.as_ref().clone(),
                })),
            }),
        }
    }
}
