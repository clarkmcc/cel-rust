use crate::functions::FromTarget;
use crate::{Argument, ExecutionError, FunctionContext, ResolveResult, Value};
use cel_parser::{Atom, Expression};
use chrono::{DateTime, Duration, FixedOffset};
use std::collections::HashMap;
use std::marker::PhantomData;
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

pub struct WithTarget;
pub struct NoTarget;

macro_rules! impl_handler {
    // Base case: no more types, with a target
    (@go_with_target $idx: expr, $call: expr, $ftx: ident, $target: ident, [], [$($args:tt)*]) => {
        $call($target, $($args)*).into_resolve_result()
    };

    // Base case: no more types, without a target
    (@go_without_target $idx: expr, $call: expr, $ftx: ident, [], [$($args:tt)*]) => {
        $call($($args)*).into_resolve_result()
    };

    // Recurse for each ident, incrementing the index each time, with a target
    (@go_with_target $idx: expr, $call: expr, $ftx: ident, $target: ident, [$head: ident, $($tail: ident,)*], [$($args:tt)*]) => {
        impl_handler!(@go_with_target $idx + 1, $call, $ftx, $target, [$($tail,)*], [
            $($args)*
            {
                let temp = $head::from(&$ftx.resolve(Argument($idx))?)?;
                temp
            },
        ])
    };

    // Recurse for each ident, incrementing the index each time, without a target
    (@go_without_target $idx: expr, $call: expr, $ftx: ident, [$head: ident, $($tail: ident,)*], [$($args:tt)*]) => {
        impl_handler!(@go_without_target $idx + 1, $call, $ftx, [$($tail,)*], [
            $($args)*
            {
                let temp = $head::from(&$ftx.resolve(Argument($idx))?)?;
                temp
            },
        ])
    };

    // Main entry point.
    ($($ty: ident),+ $(,)?) => {
        impl<F, Target, $($ty,)* R> Handler<(WithTarget, Target, $($ty,)*)> for F
        where
            F: Fn(Target, $($ty,)*) -> R + Clone + 'static,
            Target: FromContext,
            $( $ty: FromValue, )*
            R: IntoResolveResult,
        {
            fn call(self, ftx: FunctionContext) -> ResolveResult {
                let target = Target::from_context(&ftx)?;
                impl_handler!(@go_with_target 0, self, ftx, target, [$($ty,)*], [])
            }
        }

        impl<F, $($ty,)* R> Handler<(NoTarget, $($ty,)*)> for F
        where
            F: Fn($($ty,)*) -> R + Clone + 'static,
            $( $ty: FromValue, )*
            R: IntoResolveResult,
        {
            fn call(self, ftx: FunctionContext) -> ResolveResult {
                impl_handler!(@go_without_target 0, self, ftx, [$($ty,)*], [])
            }
        }
    }
}

trait FromValue {
    fn from(expr: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

impl FromValue for Value {
    fn from(expr: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        Ok(expr.clone())
    }
}

trait FromContext {
    fn from_context(ctx: &FunctionContext) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

pub struct Target<T>(pub T);

impl<T> FromContext for Target<T>
where
    T: FromValue,
{
    fn from_context(ctx: &FunctionContext) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        let value = ctx.target::<Value>()?;
        Ok(Target(T::from(value)?))
    }
}

trait IntoResolveResult {
    fn into_resolve_result(self) -> ResolveResult;
}

impl IntoResolveResult for String {
    fn into_resolve_result(self) -> ResolveResult {
        Ok(Value::String(Rc::new(self)))
    }
}

impl IntoResolveResult for Result<Value, ExecutionError> {
    fn into_resolve_result(self) -> ResolveResult {
        self
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

impl_handler!(T1);
impl_handler!(T1, T2);

// // Implementation with target
// impl<F, Target, A1, R> Handler<(WithTarget, Target, A1)> for F
// where
//     F: Fn(Target, A1) -> R + Clone + 'static,
//     Target: FromContext,
//     A1: FromValue,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: FunctionContext) -> ResolveResult {
//         let target = Target::from_context(&ftx)?;
//         let arg = ftx.resolve(Argument(0))?;
//         let arg = A1::from(&arg)?;
//         self(target, arg).into_resolve_result()
//     }
// }
//
// // Implementation without target
// impl<F, A1, R> Handler<(NoTarget, A1)> for F
// where
//     F: Fn(A1) -> R + Clone + 'static,
//     A1: FromValue,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: FunctionContext) -> ResolveResult {
//         let arg = ftx.resolve(Argument(0))?;
//         let arg = A1::from(&arg)?;
//         self(arg).into_resolve_result()
//     }
// }
//
// // Implementation with target
// impl<F, Target, A1, A2, R> Handler<(WithTarget, Target, A1, A2)> for F
// where
//     F: Fn(Target, A1, A2) -> R + Clone + 'static,
//     Target: FromContext,
//     A1: FromValue,
//     A2: FromValue,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: FunctionContext) -> ResolveResult {
//         let target = Target::from_context(&ftx)?;
//         let arg1 = ftx.resolve(Argument(0))?;
//         let arg2 = ftx.resolve(Argument(1))?;
//         let arg1 = A1::from(&arg1)?;
//         let arg2 = A2::from(&arg2)?;
//         self(target, arg1, arg2).into_resolve_result()
//     }
// }
//
// // Implementation without target
// impl<F, A1, A2, R> Handler<(NoTarget, A1, A2)> for F
// where
//     F: Fn(A1, A2) -> R + Clone + 'static,
//     A1: FromValue,
//     A2: FromValue,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: FunctionContext) -> ResolveResult {
//         let arg1 = ftx.resolve(Argument(0))?;
//         let arg2 = ftx.resolve(Argument(1))?;
//         let arg1 = A1::from(&arg1)?;
//         let arg2 = A2::from(&arg2)?;
//         self(arg1, arg2).into_resolve_result()
//     }
// }

// Heavily inspired by https://users.rust-lang.org/t/common-data-type-for-functions-with-different-parameters-e-g-axum-route-handlers/90207/6
// and https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=c6744c27c2358ec1d1196033a0ec11e4

#[derive(Default)]
pub struct FunctionRegistry {
    routes: HashMap<String, Box<dyn Function>>,
}

impl FunctionRegistry {
    pub(crate) fn add<H, T>(&mut self, name: &str, handler: H)
    where
        H: Handler<T> + 'static,
        T: 'static,
    {
        self.routes.insert(
            name.to_string(),
            Box::new(HandlerFunction {
                handler,
                into_callable: |h, ctx| Box::new(HandlerCallable::new(h, ctx)),
            }),
        );
    }

    pub(crate) fn get(&self, name: &str) -> Option<Box<dyn Function>> {
        self.routes.get(name).map(|f| f.clone_box())
    }

    pub(crate) fn has(&self, name: &str) -> bool {
        self.routes.contains_key(name)
    }

    fn call(&self, name: &str, ctx: &FunctionContext) -> ResolveResult {
        self.routes
            .get(name)
            .unwrap()
            .clone_box()
            .call_with_context(ctx)
    }
}

pub trait Function {
    fn clone_box(&self) -> Box<dyn Function>;
    fn into_callable<'a>(self: Box<Self>, ctx: &'a FunctionContext) -> Box<dyn Callable + 'a>;
    fn call_with_context<'a>(self: Box<Self>, ctx: &'a FunctionContext) -> ResolveResult;
}

pub struct HandlerFunction<H: Clone> {
    pub handler: H,
    pub into_callable: for<'a> fn(H, &'a FunctionContext) -> Box<dyn Callable + 'a>,
}

impl<H: Clone> Clone for HandlerFunction<H> {
    fn clone(&self) -> Self {
        Self {
            handler: self.handler.clone(),
            into_callable: self.into_callable,
        }
    }
}

impl<H> Function for HandlerFunction<H>
where
    H: Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn Function> {
        Box::new(self.clone())
    }

    fn into_callable<'a>(self: Box<Self>, ctx: &'a FunctionContext) -> Box<dyn Callable + 'a> {
        (self.into_callable)(self.handler, ctx)
    }

    fn call_with_context<'a>(self: Box<Self>, ctx: &'a FunctionContext) -> ResolveResult {
        self.into_callable(ctx).call()
    }
}

// Callable and HandlerCallable
pub trait Callable {
    fn call(&mut self) -> ResolveResult;
}

pub struct HandlerCallable<'a, H, T> {
    handler: H,
    context: &'a FunctionContext<'a, 'a, 'a>,
    _marker: PhantomData<fn() -> T>,
}

impl<'a, H, T> HandlerCallable<'a, H, T> {
    pub fn new(handler: H, ctx: &'a FunctionContext) -> Self {
        Self {
            handler,
            context: ctx,
            _marker: PhantomData,
        }
    }
}

impl<'a, H, T> Callable for HandlerCallable<'a, H, T>
where
    H: Handler<T> + Clone + 'static,
{
    fn call(&mut self) -> ResolveResult {
        self.handler.clone().call(self.context.clone())
    }
}

pub trait Handler<T>: Clone {
    fn call(self, ctx: FunctionContext) -> ResolveResult;
}

fn double(x: i32) -> i32 {
    x * 2
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn starts_with(Target(target): Target<Rc<String>>, name: Rc<String>) -> bool {
    target.starts_with(name.as_str())
}

#[test]
fn test() {
    let mut router = FunctionRegistry::default();
    // router.add("double", double);
    // router.add("add", add);
    router.add("starts_with", starts_with);
    let target = Value::String(Rc::new("foobar".to_string()));
    let ctx = FunctionContext {
        name: Rc::new("".to_string()),
        target: Some(&target),
        ptx: &Default::default(),
        args: &[
            Expression::Atom(Atom::String(Rc::new("foo".to_string()))),
            // Expression::Atom(Atom::Int(20)),
        ],
    };

    let result = router.call("starts_with", &ctx);
    println!("{:?}", result)
}
