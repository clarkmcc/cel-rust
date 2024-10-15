use cel_interpreter::{Context, Duration, Program, Timestamp};
use serde::Serialize;

// An example struct that derives Serialize
#[derive(Serialize)]
struct MyStruct {
    a: i32,
    b: i32,
    // To preserve durations and timestamps, use the cel_interpreter wrapper types.
    c: Duration,
    d: Timestamp,
}

fn main() {
    let mut context = Context::default();

    // MyStruct will be implicitly serialized into the CEL appropriate types
    context
        .add_variable(
            "foo",
            MyStruct {
                a: 1,
                b: 1,
                c: chrono::Duration::hours(2).into(),
                d: chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                    .unwrap()
                    .into(),
            },
        )
        .unwrap();

    let program = Program::compile("foo.a == foo.b").unwrap();
    let value = program.execute(&context).unwrap();
    assert_eq!(value, true.into());

    let program = Program::compile("foo.c == duration('2h')").unwrap();
    let value = program.execute(&context).unwrap();
    assert_eq!(value, true.into());

    let program = Program::compile("foo.d == timestamp('1996-12-19T16:39:57-08:00')").unwrap();
    let value = program.execute(&context).unwrap();
    assert_eq!(value, true.into());
}
