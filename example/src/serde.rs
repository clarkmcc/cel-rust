use cel_interpreter::{to_value, Context, Program};
use serde::Serialize;

// An example struct that derives Serialize
#[derive(Serialize)]
struct MyStruct {
    a: i32,
    b: i32,
}

fn main() {
    let program = Program::compile("foo.a == foo.b").unwrap();
    let mut context = Context::default();

    // MyStruct will be implicitly serialized into the CEL appropriate types
    context
        .add_variable("foo", MyStruct { a: 1, b: 1 })
        .unwrap();
    // To explicitly serialize structs use to_value()
    let _cel_value = to_value(MyStruct { a: 2, b: 2 }).unwrap();

    let value = program.execute(&context).unwrap();
    assert_eq!(value, true.into());
}
