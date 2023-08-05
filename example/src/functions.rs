use cel_interpreter::{Argument, Context, FunctionContext, Program, ResolveResult};

fn main() {
    let program = Program::compile("add(2, 3)").unwrap();
    let mut context = Context::default();
    context.add_function("add", add);

    let value = program.execute(&context).unwrap();
    assert_eq!(value, 5.into());
}

/// The add function takes two arguments and returns their sum.
fn add(a: i32, b: i32) -> i32 {
    a + b
}
