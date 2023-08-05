use crate::functions::FromThis;
use crate::{
    AllArguments, Argument, Context, ExecutionError, FunctionContext, ResolveResult, Value,
};
use cel_parser::{Atom, Expression};
use chrono::{DateTime, Duration, FixedOffset};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;

macro_rules! impl_from_value {
    ($target_type:ty, $value_variant:path) => {
        impl FromValue for $target_type {
            fn from_value(expr: &Value) -> Result<Self, ExecutionError> {
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
            fn from_value(expr: &Value) -> Result<Self, ExecutionError> {
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

        impl IntoResolveResult for Result<$target_type, ExecutionError> {
            fn into_resolve_result(self) -> ResolveResult {
                self.map($value_variant)
            }
        }
    };
}

macro_rules! impl_arg_value_from_context {
    ($($type:ty),*) => {
        $(
            impl<'a, 'context> FromContext<'a, 'context> for $type {
                fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
                where
                    Self: Sized,
                {
                    arg_value_from_context(ctx).and_then(|v| FromValue::from_value(&v))
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! impl_handler {
    ($($t:ty),*) => {
        paste::paste! {
            impl<F, $($t,)* R> Handler<($($t,)*)> for F
            where
                F: Fn($($t,)*) -> R + Clone,
                $($t: for<'a, 'context> FromContext<'a, 'context>,)*
                R: IntoResolveResult,
            {
                fn call(self, ftx: &mut FunctionContext) -> ResolveResult {
                    $(
                        let [<arg_ $t>] = $t::from_context(ftx)?;
                    )*
                    self($([<arg_ $t>],)*).into_resolve_result()
                }
            }

            impl<F, $($t,)* R> Handler<(WithFunctionContext, $($t,)*)> for F
            where
                F: Fn(&FunctionContext, $($t,)*) -> R + Clone,
                $($t: for<'a, 'context> FromContext<'a, 'context>,)*
                R: IntoResolveResult,
            {
                fn call(self, ftx: &mut FunctionContext) -> ResolveResult {
                    $(
                        let [<arg_ $t>] = $t::from_context(ftx)?;
                    )*
                    self(ftx, $([<arg_ $t>],)*).into_resolve_result()
                }
            }
        }
    };
}

trait FromValue {
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

trait FromContext<'a, 'context> {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

pub struct This<T>(pub T);

impl<'a, 'context, T> FromContext<'a, 'context> for This<T>
where
    T: FromValue,
{
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        let value = ctx
            .this::<Value>()
            .cloned()
            .or_else(|_| arg_value_from_context(ctx))?;
        Ok(This(T::from_value(&value)?))
    }
}

#[derive(Clone)]
pub struct Identifier(pub Rc<String>);

impl<'a, 'context> FromContext<'a, 'context> for Identifier {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        match arg_expr_from_context(ctx) {
            Expression::Ident(ident) => Ok(Identifier(ident.clone())),
            _ => {
                // todo: better error
                return Err(ExecutionError::UnexpectedType {
                    got: "not an identifier".to_string(),
                    want: "identifier".to_string(),
                });
            }
        }
    }
}

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.0.as_ref().clone()
    }
}

#[derive(Clone)]
pub struct List(pub Rc<Vec<Value>>);

impl FromValue for List {
    fn from_value(value: &Value) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        match value {
            Value::List(list) => Ok(List(list.clone())),
            _ => Err(ExecutionError::UnexpectedType {
                got: format!("{:?}", value),
                want: "list".to_string(),
            }),
        }
    }
}

#[derive(Clone)]
pub struct Arguments(pub Rc<Vec<Value>>);

impl<'a, 'context> FromContext<'a, 'context> for Arguments {
    fn from_context(ctx: &'a mut FunctionContext) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        match ctx.resolve(AllArguments)? {
            Value::List(list) => Ok(Arguments(list.clone())),
            _ => todo!(),
        }
    }
}

impl<'a, 'context> FromContext<'a, 'context> for Value {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        arg_value_from_context(ctx)
    }
}

impl<'a, 'context> FromContext<'a, 'context> for Expression {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        Ok(arg_expr_from_context(ctx))
    }
}

fn arg_expr_from_context(ctx: &mut FunctionContext) -> Expression {
    let idx = ctx.arg_idx;
    ctx.arg_idx += 1;
    ctx.args[idx].clone()
}

fn arg_value_from_context(ctx: &mut FunctionContext) -> Result<Value, ExecutionError> {
    let idx = ctx.arg_idx;
    ctx.arg_idx += 1;
    ctx.resolve(Argument(idx))
}

pub trait IntoResolveResult {
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

impl_arg_value_from_context!(
    i32,
    u32,
    f64,
    Rc<String>,
    Rc<Vec<u8>>,
    bool,
    Duration,
    DateTime<FixedOffset>,
    List
);

pub struct WithFunctionContext;

impl_handler!();
impl_handler!(C1);
impl_handler!(C1, C2);
impl_handler!(C1, C2, C3);
impl_handler!(C1, C2, C3, C4);

// impl<F, C1, C2, R> Handler<(C1, C2)> for F
// where
//     F: Fn(C1, C2) -> R + Clone,
//     C1: for<'a, 'context> FromContext<'a, 'context>,
//     C2: for<'a, 'context> FromContext<'a, 'context>,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: &mut FunctionContext) -> ResolveResult {
//         let arg1 = C1::from_context(ftx)?;
//         let arg2 = C2::from_context(ftx)?;
//         self(arg1, arg2).into_resolve_result()
//     }
// }

// impl<F, C1, C2, R> Handler<(WithFunctionContext, C1, C2)> for F
// where
//     F: Fn(&FunctionContext, C1, C2) -> R + Clone,
//     C1: for<'a, 'context> FromContext<'a, 'context>,
//     C2: for<'a, 'context> FromContext<'a, 'context>,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: &mut FunctionContext) -> ResolveResult {
//         let arg1 = C1::from_context(ftx)?;
//         let arg2 = C2::from_context(ftx)?;
//         self(ftx, arg1, arg2).into_resolve_result()
//     }
// }

// impl<F, C1, R> Handler<(C1)> for F
// where
//     F: Fn(C1) -> R + Clone + 'static,
//     C1: for<'a, 'context> FromContext<'a, 'context>,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: &mut FunctionContext) -> ResolveResult {
//         let arg1 = C1::from_context(ftx)?;
//         self(arg1).into_resolve_result()
//     }
// }

// impl<F, C1, C2, C3, R> Handler<(WithFunctionContext, C1, C2, C3)> for F
// where
//     F: Fn(&FunctionContext, C1, C2, C3) -> R + Clone + 'static,
//     C1: for<'a, 'context> FromContext<'a, 'context>,
//     C2: for<'a, 'context> FromContext<'a, 'context>,
//     C3: for<'a, 'context> FromContext<'a, 'context>,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: &mut FunctionContext) -> ResolveResult {
//         let arg1 = C1::from_context(ftx)?;
//         let arg2 = C2::from_context(ftx)?;
//         let arg3 = C3::from_context(ftx)?;
//         self(ftx, arg1, arg2, arg3).into_resolve_result()
//     }
// }

// impl<F, C1, C2, C3, C4, R> Handler<(WithFunctionContext, C1, C2, C3, C4)> for F
// where
//     F: Fn(&FunctionContext, C1, C2, C3, C4) -> R + Clone + 'static,
//     C1: for<'a, 'context> FromContext<'a, 'context>,
//     C2: for<'a, 'context> FromContext<'a, 'context>,
//     C3: for<'a, 'context> FromContext<'a, 'context>,
//     C4: for<'a, 'context> FromContext<'a, 'context>,
//     R: IntoResolveResult,
// {
//     fn call(self, ftx: &mut FunctionContext) -> ResolveResult {
//         let arg1 = C1::from_context(ftx)?;
//         let arg2 = C2::from_context(ftx)?;
//         let arg3 = C3::from_context(ftx)?;
//         let arg4 = C4::from_context(ftx)?;
//         self(ftx, arg1, arg2, arg3, arg4).into_resolve_result()
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

    fn call(&self, name: &str, ctx: &mut FunctionContext) -> ResolveResult {
        self.routes
            .get(name)
            .unwrap()
            .clone_box()
            .call_with_context(ctx)
    }
}

pub trait Function {
    fn clone_box(&self) -> Box<dyn Function>;
    fn into_callable<'a>(self: Box<Self>, ctx: &'a mut FunctionContext) -> Box<dyn Callable + 'a>;
    fn call_with_context<'a>(self: Box<Self>, ctx: &'a mut FunctionContext) -> ResolveResult;
}

pub struct HandlerFunction<H: Clone> {
    pub handler: H,
    pub into_callable: for<'a> fn(H, &'a mut FunctionContext) -> Box<dyn Callable + 'a>,
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

    fn into_callable<'a>(self: Box<Self>, ctx: &'a mut FunctionContext) -> Box<dyn Callable + 'a> {
        (self.into_callable)(self.handler, ctx)
    }

    fn call_with_context<'a>(self: Box<Self>, ctx: &'a mut FunctionContext) -> ResolveResult {
        self.into_callable(ctx).call()
    }
}

// Callable and HandlerCallable
pub trait Callable {
    fn call(&mut self) -> ResolveResult;
}

pub struct HandlerCallable<'a, 'context, H, T> {
    handler: H,
    context: &'a mut FunctionContext<'context>,
    _marker: PhantomData<fn() -> T>,
}

impl<'a, 'context, H, T> HandlerCallable<'a, 'context, H, T> {
    pub fn new(handler: H, ctx: &'a mut FunctionContext<'context>) -> Self {
        Self {
            handler,
            context: ctx,
            _marker: PhantomData,
        }
    }
}

impl<'a, 'context, H, T> Callable for HandlerCallable<'a, 'context, H, T>
where
    H: Handler<T> + Clone + 'static,
{
    fn call(&mut self) -> ResolveResult {
        self.handler.clone().call(self.context)
    }
}

pub trait Handler<T>: Clone {
    fn call(self, ctx: &mut FunctionContext) -> ResolveResult;
}

fn double(x: i32) -> i32 {
    x * 2
}

// fn add(a: i32, b: i32) -> i32 {
//     a + b
// }

fn starts_with(ftx: &FunctionContext, This(this): This<Rc<String>>, prefix: Rc<String>) -> bool {
    this.starts_with(prefix.as_str())
}

// fn starts_with(This(this): This<Rc<String>>, name: Rc<String>) -> bool {
//     this.starts_with(name.as_str())
// }

#[test]
fn test() {
    let mut router = FunctionRegistry::default();
    // router.add("double", double);
    // router.add("add", add);
    router.add("starts_with", starts_with);
    let target = Value::String(Rc::new("foobar".to_string()));
    let mut ctx = FunctionContext {
        name: Rc::new("".to_string()),
        // target: Some(&target),
        this: None,
        ptx: &Default::default(),
        args: vec![
            Expression::Atom(Atom::String(Rc::new("foobar".to_string()))).into(),
            Expression::Atom(Atom::String(Rc::new("foo".to_string()))).into(),
            // Expression::Atom(Atom::Int(20)),
            // Expression::Atom(Atom::Int(10)),
        ]
        .into(),
        arg_idx: 0,
    };

    let result = router.call("starts_with", &mut ctx);
    println!("{:?}", result)
}
