use crate::{Argument, ExecutionError, FunctionContext, ResolveResult, Value};
use cel_parser::{Atom, Expression};
use chrono::{DateTime, Duration, FixedOffset};
use std::rc::Rc;

macro_rules! impl_from_value {
    ($target_type:ty, $value_variant:path) => {
        impl FromValue for $target_type {
            fn from(expr: &Value) -> Result<Self, ExecutionError> {
                if let $value_variant(v) = expr {
                    Ok(v.clone())
                } else {
                    Err(ExecutionError::UnexpectedType {
                        got: format!("{:?}", expr),
                        want: stringify!($target_type).to_string(),
                    })
                }
            }
        }

        impl FromValue for Option<$target_type> {
            fn from(expr: &Value) -> Result<Self, ExecutionError> {
                match expr {
                    Value::Null => Ok(None),
                    $value_variant(v) => Ok(Some(v.clone())),
                    _ => Err(ExecutionError::UnexpectedType {
                        got: format!("{:?}", expr),
                        want: stringify!($target_type).to_string(),
                    }),
                }
            }
        }

        impl From<$target_type> for Value {
            fn from(value: $target_type) -> Self {
                $value_variant(value)
            }
        }

        impl IntoResolveResult for $target_type {
            fn into_resolve_result(self) -> ResolveResult {
                Ok($value_variant(self))
            }
        }
    };
}

// impl<F, T1, T2> Callable<(T1, T2)> for F
// where
//     F: Fn(T1, T2),
//     T1: FromValue,
//     T2: FromValue,
// {
//     fn call(&self, ftx: FunctionContext) -> ResolveResult {
//         let arg1 = ftx.resolve(Argument(0))?;
//         let arg2 = ftx.resolve(Argument(1))?;
//         let t1 = T1::from(&arg1)?;
//         let t2 = T2::from(&arg2)?;
//         self(t1, t2);
//         Ok(Value::Null)
//     }
// }

// macro_rules! impl_callable {
//     ($($ty: ident),+) => {
//         impl<F, $($ty,)* R> Callable<($($ty,)*)> for F
//         where
//             F: Fn($($ty,)*) -> R + 'static,
//             $( $ty: FromValue, )*
//             R: IntoResolveResult,
//         {
//             fn call(&self, ftx: FunctionContext) -> ResolveResult {
//                 $(let $ty = $ty::from(&ftx.resolve(Argument(0))?)?;)*
//                 self($($ty),*).into_resolve_result()
//             }
//         }
//     }
// }

macro_rules! impl_callable {
    // Base case: no more types.
    (@go $idx: expr, [], $call: expr, $ftx: ident, [$($args:tt)*]) => {
        $call($($args)*).into_resolve_result()
    };

    // Recurse for each ident, incrementing the index each time.
    (@go $idx: expr, [$head: ident, $($tail: ident,)*], $call: expr, $ftx: ident, [$($args:tt)*]) => {
        impl_callable!(@go $idx + 1, [$($tail,)*], $call, $ftx, [
            $($args)*
            {
                let temp = $head::from(&$ftx.resolve(Argument($idx))?)?;
                temp
            },
        ])
    };

    // Main entry point.
    ($($ty: ident),+ $(,)?) => {
        impl<F, $($ty,)* R> Callable<($($ty,)*)> for F
        where
            F: Fn($($ty,)*) -> R + 'static,
            $( $ty: FromValue, )*
            R: IntoResolveResult,
        {
            fn call(&self, ftx: FunctionContext) -> ResolveResult {
                impl_callable!(@go 0, [$($ty,)*], self, ftx, [])
            }
        }
    }
}

pub(crate) trait Callable<T> {
    fn call(&self, ftx: FunctionContext) -> ResolveResult;
}

trait FromValue {
    fn from(expr: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

trait IntoResolveResult {
    fn into_resolve_result(self) -> ResolveResult;
}

impl IntoResolveResult for String {
    fn into_resolve_result(self) -> ResolveResult {
        Ok(Value::String(Rc::new(self)))
    }
}

impl_from_value!(i32, Value::Int);
impl_from_value!(u32, Value::UInt);
impl_from_value!(f64, Value::Float);
impl_from_value!(Rc<String>, Value::String);
impl_from_value!(Rc<Vec<u8>>, Value::Bytes);
impl_from_value!(bool, Value::Bool);
impl_from_value!(Duration, Value::Duration);
impl_from_value!(DateTime<FixedOffset>, Value::Timestamp);

impl_callable!(T1);
impl_callable!(T1, T2);

fn call_function<T, H>(function: H) -> ResolveResult
where
    H: Callable<T>,
{
    function.call(FunctionContext {
        name: Rc::new("".to_string()),
        target: None,
        ptx: &Default::default(),
        args: &[
            Expression::Atom(Atom::UInt(10)),
            Expression::Atom(Atom::UInt(20)),
            // Expression::Atom(Atom::Null),
            // Expression::Atom(Atom::String(Rc::new("world".to_string()))),
        ],
    })
}

fn add(a: u32, b: u32) -> u32 {
    a + b
}

#[test]
fn test_foo() {
    let result = call_function(add);
    // let result = call_function(greet);
    println!("{:?}", result);
}
