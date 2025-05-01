use crate::{Context, Program, Value, CelType};

#[test]
fn test_type_fn_basic() {
    let ctx = Context::default();
    // int
    let prog = Program::compile("type(1)").unwrap();
    let result = prog.execute(&ctx).unwrap();
    assert_eq!(result, Value::Type(CelType::Int));
    // string
    let prog = Program::compile("type('a')").unwrap();
    let result = prog.execute(&ctx).unwrap();
    assert_eq!(result, Value::Type(CelType::String));
    // bool
    let prog = Program::compile("type(true)").unwrap();
    let result = prog.execute(&ctx).unwrap();
    assert_eq!(result, Value::Type(CelType::Bool));
}

#[test]
fn test_type_fn_type_of_type() {
    let ctx = Context::default();
    let prog = Program::compile("type(type(1))").unwrap();
    let result = prog.execute(&ctx).unwrap();
    assert_eq!(result, Value::Type(CelType::Type));
}

#[test]
fn test_type_fn_equality() {
    let ctx = Context::default();
    let prog = Program::compile("type(1) == int").unwrap();
    let result = prog.execute(&ctx).unwrap();
    assert_eq!(result, Value::Bool(false));

    let prog = Program::compile("type(type(1)) == type(int)").unwrap();
    let result = prog.execute(&ctx).unwrap();
    assert_eq!(result, Value::Bool(true));
}
