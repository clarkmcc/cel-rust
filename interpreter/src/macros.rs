#[macro_export]
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

            impl $crate::magic::IntoResolveResult for $target_type {
                fn into_resolve_result(self) -> ResolveResult {
                    Ok($value_variant(self $(as $cast)?))
                }
            }

            impl $crate::magic::IntoResolveResult for Result<$target_type, ExecutionError> {
                fn into_resolve_result(self) -> ResolveResult {
                    self.map(|it| $value_variant(it $(as $cast)?))
                }
            }

            impl<'a, 'context> FromContext<'a, 'context> for $target_type {
                fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
                where
                    Self: Sized,
                {
                    arg_value_from_context(ctx).and_then(|v| FromValue::from_value(&v))
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! impl_handler {
    ($($t:ty),*) => {
        paste::paste! {
            impl<F, $($t,)* R> Handler<($($t,)*)> for F
            where
                F: Fn($($t,)*) -> R + Clone,
                $($t: for<'a, 'context> $crate::FromContext<'a, 'context>,)*
                R: IntoResolveResult,
            {
                fn call(self, _ftx: &mut FunctionContext) -> ResolveResult {
                    $(
                        let [<arg_ $t:lower>] = $t::from_context(_ftx)?;
                    )*
                    self($([<arg_ $t:lower>],)*).into_resolve_result()
                }
            }

            impl<F, $($t,)* R> Handler<(WithFunctionContext, $($t,)*)> for F
            where
                F: Fn(&FunctionContext, $($t,)*) -> R + Clone,
                $($t: for<'a, 'context> $crate::FromContext<'a, 'context>,)*
                R: IntoResolveResult,
            {
                fn call(self, _ftx: &mut FunctionContext) -> ResolveResult {
                    $(
                        let [<arg_ $t:lower>] = $t::from_context(_ftx)?;
                    )*
                    self(_ftx, $([<arg_ $t:lower>],)*).into_resolve_result()
                }
            }
        }
    };
}

pub(crate) use impl_conversions;
pub(crate) use impl_handler;
