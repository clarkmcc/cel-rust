use super::*;

/// Describes any type that can be converted from a [`Value`] into itself.
/// This is commonly used to convert from [`Value`] into primitive types,
/// e.g. from `Value::Bool(true) -> true`. This trait is auto-implemented
/// for many CEL-primitive types.
pub(crate) trait FromValue {
    fn from_value(value: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

impl FromValue for Value {
    fn from_value(value: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        Ok(value.clone())
    }
}

macro_rules! impl_conversions {
    // Capture pairs separated by commas, where each pair is separated by =>
    ($($target_type:ty $(as $cast:ty)? => $value_variant:path),* $(,)?) => {
        $(
            impl FromValue for $target_type {
                fn from_value(expr: &Value) -> Result<Self, ExecutionError> {
                    if let $value_variant(v) = expr {
                        $(if <$target_type>::MAX as $cast < *v {
                            return Err(ExecutionError::CastOverflow {
                                value: *v as f64,
                                source_ty: std::any::type_name::<$cast>(),
                                target_ty: std::any::type_name::<$target_type>(),
                            })
                        } else if <$target_type>::MIN as $cast > *v {
                            return Err(ExecutionError::CastOverflow {
                                value: *v as f64,
                                source_ty: std::any::type_name::<$cast>(),
                                target_ty: std::any::type_name::<$target_type>(),
                            })
                        })?
                        Ok(v.clone() $(as $cast as $target_type)?)
                    } else {
                        Err(ExecutionError::UnexpectedType {
                            got: format!("{:?}", expr),
                            want: stringify!($target_type).to_string(),
                        })
                    }
                }
            }

            impl FromValue for Option<$target_type> {
                fn from_value(expr: &Value) -> Result<Self, ExecutionError> {
                    match expr {
                        Value::Null => Ok(None),
                        $value_variant(v) => {
                            $(if <$target_type>::MAX as $cast < *v {
                                return Err(ExecutionError::CastOverflow {
                                    value: *v as f64,
                                    source_ty: std::any::type_name::<$cast>(),
                                    target_ty: std::any::type_name::<$target_type>(),
                                })
                            } else if <$target_type>::MIN as $cast > *v {
                                return Err(ExecutionError::CastOverflow {
                                    value: *v as f64,
                                    source_ty: std::any::type_name::<$cast>(),
                                    target_ty: std::any::type_name::<$target_type>(),
                                })
                            })?
                            Ok(Some(v.clone() $(as $cast as $target_type)?))
                        },
                        _ => Err(ExecutionError::UnexpectedType {
                            got: format!("{:?}", expr),
                            want: stringify!($target_type).to_string(),
                        }),
                    }
                }
            }

            impl From<$target_type> for Value {
                fn from(value: $target_type) -> Self {
                    $value_variant(value $(as $cast)?)
                }
            }

            impl crate::magic::IntoResolveResult for $target_type {
                fn into_resolve_result(self) -> ResolveResult {
                    Ok($value_variant(self $(as $cast)?))
                }
            }

            impl crate::magic::IntoResolveResult for Result<$target_type, ExecutionError> {
                fn into_resolve_result(self) -> ResolveResult {
                    self.map(|it| $value_variant(it $(as $cast)?))
                }
            }

            impl<'a, 'context> crate::magic::FromContext<'a, 'context> for $target_type {
                fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
                where
                    Self: Sized,
                {
                    crate::magic::arg_value_from_context(ctx).and_then(|v| FromValue::from_value(&v))
                }
            }
        )*
    }
}
impl_conversions!(
    i8 as i64 => Value::Int,
    i16 as i64 => Value::Int,
    i32 as i64 => Value::Int,
    i64 => Value::Int,
    u8 as u64 => Value::UInt,
    u16 as u64 => Value::UInt,
    u32 as u64 => Value::UInt,
    u64 => Value::UInt,
    f32 as f64 => Value::Float,
    f64 => Value::Float,
    Arc<String> => Value::String,
    Arc<Vec<u8>> => Value::Bytes,
    bool => Value::Bool,
    Arc<Vec<Value>> => Value::List
);

#[cfg(feature = "chrono")]
impl_conversions!(
    chrono::Duration => Value::Duration,
    chrono::DateTime<chrono::FixedOffset> => Value::Timestamp,
);
