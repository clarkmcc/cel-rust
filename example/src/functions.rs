use cel_interpreter::extractors::This;
use cel_interpreter::{Context, ExecutionError, FunctionContext, Program, ResolveResult};
use std::rc::Rc;

fn main() {
    let program = Program::compile("add(2, 3) == 5 && ''.isEmpty() && fail()").unwrap();
    let mut context = Context::default();

    // Add functions using closures
    context.add_function("add", |a: i32, b: i32| a + b);

    // Add methods to a string type
    context.add_function("isEmpty", is_empty);

    // Use the function context to return error messages
    context.add_function("fail", fail);

    // Run the program
    let result = program.execute(&context);
    assert!(matches!(result, Err(ExecutionError::FunctionError { .. })));
}

/// A method on a string type. When added to the CEL context, this function
/// can be called by running. We use the [`This`] extractor give us a reference
/// to the string that this method was called on.
///
/// ```skip
/// "foo".isEmpty()
/// ```
fn is_empty(This(s): This<Rc<String>>) -> bool {
    s.is_empty()
}

/// A function that gives us access to the [`FunctionContext`]. All functions have
/// can accept this context as their first argument. The context is helpful for
/// several reasons:
/// 1. Creating shadowed variables using cloned contexts (see the `filter` and `map`
///    functions for example).
/// 2. Functions that are fallible can return an error message using ftx.error(...)
///    which attaches some helpful context for the error.
fn fail(ftx: &FunctionContext) -> ResolveResult {
    ftx.error("This function always fails").into()
}
