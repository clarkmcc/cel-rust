use cel_interpreter::context::Context;
use cel_interpreter::objects::CelType;
use cel_interpreter::Program;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::collections::HashMap;

pub fn criterion_benchmark(c: &mut Criterion) {
    let expressions = vec![
        ("benchmark", "((TestDouble >= 1.0 || TestString.TestFunction() == 'HelloWorld') && (TestDouble + 1.0 >= 0.0)) || Now > TestTime"),

        ("ternary_1", "(1 || 2) ? 1 : 2"),
        ("ternary_2", "(1 ? 2 : 3) ? 1 : 2"),
        ("or_1", "1 || 2"),
        ("and_1", "1 && 2"),
        ("and_2", "1 && (false ? 2 : 3)"),
        ("number", "1"),
        ("construct_list", "[1,2,3]"),
        ("construct_list_1", "[1]"),
        ("construct_list_2", "[1, 2]"),
        ("add_list", "[1,2,3] + [4, 5, 6]"),
        ("list_element", "[1,2,3][1]"),
        ("construct_dict", "{1: 2, '3': '4'}"),
        ("add_string", "'abc' + 'def'"),
        ("list", "[1,2,3, Now, ]"),
        ("mapexpr", "{1 + a: 3}"),
        ("map_merge", "{'a': 1} + {'a': 2, 'b': 3}"),
        ("size_list", "[1].size()"),
        ("size_list_1", "size([1])"),
        ("size_str", "'a'.size()"),
        ("size_str_2", "size('a')"),
        ("size_map", "{1:2}.size()"),
        ("size_map_2", "size({1:2})"),
        ("map has", "has(foo.bar.baz)"),
        ("map macro", "[1, 2, 3].map(x, x * 2)"),
        // ("complex", "Account{user_id: 123}.user_id == 123"),
    ];
    // https://gist.github.com/rhnvrm/db4567fcd87b2cb8e997999e1366d406

    for (name, expr) in black_box(&expressions) {
        c.bench_function(name, |b| {
            let program = Program::compile(expr).expect("Parsing failed");
            let mut ctx = Context::default();
            ctx.add_variable("TestDouble", CelType::Float(0.0f64));
            ctx.add_variable("TestString", CelType::String("World".to_string().into()));
            ctx.add_variable("TestTime", CelType::UInt(0));
            ctx.add_variable("Now", CelType::UInt(1));
            ctx.add_variable("foo", HashMap::from([("bar", 1)]));
            ctx.add_function("TestFunction", |target, _, _| match target {
                Some(CelType::String(v)) => Ok(CelType::String(format!("Hello{}", v).into())),
                _ => unreachable!(),
            });
            b.iter(|| program.execute(&ctx))
        });
        // c.bench_function(format!("{}-parsing", name).as_str(), |b| {
        //     b.iter(|| Program::compile(expr).expect("Parsing failed"))
        // });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
