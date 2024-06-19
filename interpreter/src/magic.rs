use crate::macros::{impl_conversions, impl_handler};
use crate::resolvers::{AllArguments, Argument};
use crate::{ExecutionError, FunctionContext, ResolveResult, Value};
use cel_parser::Expression;
use chrono::{DateTime, Duration, FixedOffset};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;

impl_conversions!(
    i64 => Value::Int,
    u64 => Value::UInt,
    f64 => Value::Float,
    Arc<String> => Value::String,
    Arc<Vec<u8>> => Value::Bytes,
    bool => Value::Bool,
    Duration => Value::Duration,
    DateTime<FixedOffset> => Value::Timestamp,
    Arc<Vec<Value>> => Value::List
);

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Int(value as i64)
    }
}

/// Describes any type that can be converted from a [`Value`] into itself.
/// This is commonly used to convert from [`Value`] into primitive types,
/// e.g. from `Value::Bool(true) -> true`. This trait is auto-implemented
/// for many CEL-primitive types.
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

/// A trait for types that can be converted into a [`ResolveResult`]. Every function that can
/// be registered to the CEL context must return a value that implements this trait.
pub trait IntoResolveResult {
    fn into_resolve_result(self) -> ResolveResult;
}

impl IntoResolveResult for String {
    fn into_resolve_result(self) -> ResolveResult {
        Ok(Value::String(Arc::new(self)))
    }
}

impl IntoResolveResult for Result<Value, ExecutionError> {
    fn into_resolve_result(self) -> ResolveResult {
        self
    }
}

/// Describes any type that can be converted from a [`FunctionContext`] into
/// itself, for example CEL primitives implement this trait to allow them to
/// be used as arguments to functions. This trait is core to the 'magic function
/// parameter' system. Every argument to a function that can be registered to
/// the CEL context must implement this type.
pub(crate) trait FromContext<'a, 'context> {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized;
}

/// A function argument abstraction enabling dynamic method invocation on a
/// target instance or on the first argument if the function is not called
/// as a method. This is similar to how methods can be called as functions
/// using the [fully-qualified syntax](https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#fully-qualified-syntax-for-disambiguation-calling-methods-with-the-same-name).
///
/// # Using `This`
/// ```
/// # use std::sync::Arc;
/// # use cel_interpreter::{Program, Context};
/// use cel_interpreter::extractors::This;
/// # let mut context = Context::default();
/// # context.add_function("startsWith", starts_with);
///
/// /// Notice how `This` refers to the target value when called as a method,
/// /// but the first argument when called as a function.
/// let program1 = "'foobar'.startsWith('foo') == true";
/// let program2 = "startsWith('foobar', 'foo') == true";
/// # let program1 = Program::compile(program1).unwrap();
/// # let program2 = Program::compile(program2).unwrap();
/// # let value = program1.execute(&context).unwrap();
/// # assert_eq!(value, true.into());
/// # let value = program2.execute(&context).unwrap();
/// # assert_eq!(value, true.into());
///
/// fn starts_with(This(this): This<Arc<String>>, prefix: Arc<String>) -> bool {
///     this.starts_with(prefix.as_str())
/// }
/// ```
///
/// # Type of `This`
/// This also accepts a type `T` which determines the specific type
/// that's extracted. Any type that supports [`FromValue`] can be used.
/// In the previous example, the method `startsWith` is only ever called
/// on a string, so we can use `This<Rc<String>>` to extract the string
/// automatically prior to our method actually being called.
///
/// In some cases, you may want access to the raw [`Value`] instead, for
/// example, the `contains` method works for several different types. In these
/// cases, you can use `This<Value>` to extract the raw value.
///
/// ```skip
/// pub fn contains(This(this): This<Value>, arg: Value) -> Result<Value> {
///     Ok(match this {
///         Value::List(v) => v.contains(&arg),
///         ...
///     }
/// }
/// ```
pub struct This<T>(pub T);

impl<'a, 'context, T> FromContext<'a, 'context> for This<T>
where
    T: FromValue,
{
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        if let Some(ref this) = ctx.this {
            Ok(This(T::from_value(this)?))
        } else {
            let arg = arg_value_from_context(ctx)
                .map_err(|_| ExecutionError::missing_argument_or_target())?;
            Ok(This(T::from_value(&arg)?))
        }
    }
}

/// Identifier is an argument extractor that attempts to extract an identifier
/// from an argument's expression. It fails if the argument is not available,
/// or if the argument cannot be converted into an expression.
///
/// # Examples
/// Identifiers are useful for functions like `.map` or `.filter` where one
/// of the arguments is the declaration of a variable. In this case, as noted
/// below, the x is an identifier, and we want to be able to parse it
/// automatically.
///
/// ```javascript
/// //        Identifier
/// //            ↓
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
/// ```
///
/// The function signature for the Rust implementation of `map` looks like this
///
/// ```skip
/// pub fn map(
///     ftx: &FunctionContext,
///     This(this): This<Value>, // <- [1, 2, 3]
///     ident: Identifier,       // <- x
///     expr: Expression,        // <- x * 2
/// ) -> Result<Value>;
/// ```
#[derive(Clone)]
pub struct Identifier(pub Arc<String>);

impl<'a, 'context> FromContext<'a, 'context> for Identifier {
    fn from_context(ctx: &'a mut FunctionContext<'context>) -> Result<Self, ExecutionError>
    where
        Self: Sized,
    {
        match arg_expr_from_context(ctx) {
            Expression::Ident(ident) => Ok(Identifier(ident.clone())),
            expr => Err(ExecutionError::UnexpectedType {
                got: format!("{:?}", expr),
                want: "identifier".to_string(),
            }),
        }
    }
}

impl From<&Identifier> for String {
    fn from(value: &Identifier) -> Self {
        value.0.to_string()
    }
}

impl From<Identifier> for String {
    fn from(value: Identifier) -> Self {
        value.0.as_ref().clone()
    }
}

/// An argument extractor that extracts all the arguments passed to a function, resolves their
/// expressions and returns a vector of [`Value`]. This is useful for functions that accept a
/// variable number of arguments rather than known arguments and types (for example a `sum`
/// function).
///
/// # Example
/// ```javascript
/// sum(1, 2.0, uint(3)) == 5.0
/// ```
///
/// ```rust
/// # use cel_interpreter::{Value};
/// use cel_interpreter::extractors::Arguments;
/// pub fn sum(Arguments(args): Arguments) -> Value {
///     args.iter().fold(0.0, |acc, val| match val {
///         Value::Int(x) => *x as f64 + acc,
///         Value::UInt(x) => *x as f64 + acc,
///         Value::Float(x) => *x + acc,
///         _ => acc,
///     }).into()
/// }
/// ```
#[derive(Clone)]
pub struct Arguments(pub Arc<Vec<Value>>);

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

/// Returns the next argument specified by the context's `arg_idx` field as an expression
/// (i.e. not resolved). Calling this multiple times will increment the `arg_idx` which will
/// return subsequent arguments every time.
///
/// Calling this function when there are no more arguments will result in a panic. Since this
/// function is only ever called within the context of a controlled macro that calls it once
/// for each argument, this should never happen.
fn arg_expr_from_context(ctx: &mut FunctionContext) -> Expression {
    let idx = ctx.arg_idx;
    ctx.arg_idx += 1;
    ctx.args[idx].clone()
}

/// Returns the next argument specified by the context's `arg_idx` field as after resolving
/// it. Calling this multiple times will increment the `arg_idx` which will return subsequent
/// arguments every time.
///
/// Calling this function when there are no more arguments will result in a panic. Since this
/// function is only ever called within the context of a controlled macro that calls it once
/// for each argument, this should never happen.
fn arg_value_from_context(ctx: &mut FunctionContext) -> Result<Value, ExecutionError> {
    let idx = ctx.arg_idx;
    ctx.arg_idx += 1;
    ctx.resolve(Argument(idx))
}

pub struct WithFunctionContext;

impl_handler!();
impl_handler!(C1);
impl_handler!(C1, C2);
impl_handler!(C1, C2, C3);
impl_handler!(C1, C2, C3, C4);
impl_handler!(C1, C2, C3, C4, C5);
impl_handler!(C1, C2, C3, C4, C5, C6);
impl_handler!(C1, C2, C3, C4, C5, C6, C7);
impl_handler!(C1, C2, C3, C4, C5, C6, C7, C8);
impl_handler!(C1, C2, C3, C4, C5, C6, C7, C8, C9);

// Heavily inspired by https://users.rust-lang.org/t/common-data-type-for-functions-with-different-parameters-e-g-axum-route-handlers/90207/6
// and https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=c6744c27c2358ec1d1196033a0ec11e4

#[derive(Default)]
pub struct FunctionRegistry {
    functions: HashMap<String, Box<dyn Function>>,
}

impl FunctionRegistry {
    pub(crate) fn add<H, T>(&mut self, name: &str, handler: H)
    where
        H: Handler<T> + 'static,
        T: 'static,
    {
        self.functions.insert(
            name.to_string(),
            Box::new(HandlerFunction {
                handler,
                into_callable: |h, ctx| Box::new(HandlerCallable::new(h, ctx)),
            }),
        );
    }

    pub(crate) fn get(&self, name: &str) -> Option<Box<dyn Function>> {
        self.functions.get(name).map(|f| f.clone_box())
    }

    pub(crate) fn has(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}

pub trait Function {
    fn clone_box(&self) -> Box<dyn Function>;
    fn into_callable<'a>(self: Box<Self>, ctx: &'a mut FunctionContext) -> Box<dyn Callable + 'a>;
    fn call_with_context(self: Box<Self>, ctx: &mut FunctionContext) -> ResolveResult;
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

    fn call_with_context(self: Box<Self>, ctx: &mut FunctionContext) -> ResolveResult {
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
