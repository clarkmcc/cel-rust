use cel_interpreter::{Context, FunctionContext, Program, ResolveResult};

fn main() {
    let program = Program::compile("add(2, 3)").unwrap();
    let mut context = Context::default();
    context.add_function("add", add);

    let value = program.execute(&context).unwrap();
    assert_eq!(value, 5.into());
}

/// The add function takes two arguments and returns their sum. We discard the first
/// parameter because the add function is not a method, it is always called with two
/// arguments.
fn add(ftx: FunctionContext) -> ResolveResult {
    let a = ftx.resolve_arg(0)?;
    let b = ftx.resolve_arg(1)?;
    Ok(a + b)
}
