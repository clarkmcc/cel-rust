use crate::{ExecutionError, FunctionContext, ResolveResult, Value};
use cel_parser::Expression;

pub trait Resolver {
    fn resolve(&self, ctx: &FunctionContext) -> ResolveResult;
}

/// Argument is a [`Resolver`] that resolves to the nth argument. Previously
/// users would have to call a separate method `resolve_arg` and provide the
/// arg that they wanted, but this is now handled by the [`Resolver`] trait.
///
/// # Example
/// ```
/// # use cel_interpreter::{Argument, Context, FunctionContext, Program, ResolveResult};
/// #
/// # let program = Program::compile("add(2, 3)").unwrap();
/// # let mut context = Context::default();
/// # context.add_function("add", add);
/// #
/// # let value = program.execute(&context).unwrap();
/// # assert_eq!(value, 5.into());
///
/// /// The add function takes two arguments and returns their sum. We discard the first
/// /// parameter because the add function is not a method, it is always called with two
/// /// arguments.
/// fn add(ftx: FunctionContext) -> ResolveResult {
///     let a = ftx.resolve(Argument(0))?;
///     let b = ftx.resolve(Argument(1))?;
///     Ok(a + b)
/// }
/// ```
pub struct Argument(pub usize);

impl Resolver for Argument {
    fn resolve(&self, ctx: &FunctionContext) -> ResolveResult {
        let index = self.0;
        let arg = ctx
            .args
            .get(index)
            .ok_or(ExecutionError::invalid_argument_count(
                index + 1,
                ctx.args.len(),
            ))?;
        Value::resolve(arg, ctx.ptx)
    }
}

pub struct ExpressionResolver<'a>(&'a Expression);

impl Resolver for ExpressionResolver<'_> {
    fn resolve(&self, ctx: &FunctionContext) -> ResolveResult {
        Value::resolve(self.0, ctx.ptx)
    }
}

impl<'a> From<&'a Expression> for ExpressionResolver<'a> {
    fn from(expr: &'a Expression) -> Self {
        ExpressionResolver(expr)
    }
}

/// A resolver for all arguments passed to a function. Each argument will be
/// resolved and then returned as a [`Value::List`]
///
/// # Example
/// ```
/// # use cel_interpreter::{Argument, Arguments, Context, FunctionContext, Program, ResolveResult};
/// #
/// # let program = Program::compile("list(1, 2, 3)").unwrap();
/// # let mut context = Context::default();
/// # context.add_function("list", list);
/// #
/// # let value = program.execute(&context).unwrap();
/// # assert_eq!(value, vec![1, 2, 3].into());
///
/// /// The list function takes all the provided arguments and returns them as a list.
/// fn list(ftx: FunctionContext) -> ResolveResult {
///     ftx.resolve(Arguments)
/// }
/// ```
pub struct Arguments;

impl Resolver for Arguments {
    fn resolve(&self, ctx: &FunctionContext) -> ResolveResult {
        let mut args = Vec::with_capacity(ctx.args.len());
        for arg in ctx.args {
            args.push(Value::resolve(arg, ctx.ptx)?);
        }
        Ok(Value::List(args.into()))
    }
}
