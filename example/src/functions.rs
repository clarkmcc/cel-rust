use cel_interpreter::{CelType, Context, Expression, Program, ResolveResult};

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
fn add(_: Option<&CelType>, args: &[Expression], ctx: &Context) -> ResolveResult {
    let a = CelType::resolve(args.get(0).unwrap(), ctx)?;
    let b = CelType::resolve(args.get(1).unwrap(), ctx)?;
    Ok(a + b)
}
