use common_expression_language::Program;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let expressions = vec![
        // ("ternary_1", "(1 || 2) ? 1 : 2"),
        // ("ternary_2", "(1 ? 2 : 3) ? 1 : 2"),
        // ("or_1", "1 || 2"),
        // ("and_1", "1 && 2"),
        // ("and_2", "1 && (false ? 2 : 3)"),
        // ("number", "1"),
        // ("construct_list", "[1,2,3]"),
        // ("construct_list_1", "[1]"),
        // ("construct_list_2", "[1, 2]"),
        // ("add_list", "[1,2,3] + [4, 5, 6]"),
        // ("list_element", "[1,2,3][1]"),
        // ("construct_dict", "{1: 2, '3': '4'}"),
        ("add_string", "'abc' + 'def'"),
        // ("call", "a.b(c + 3 / 2,) == 2"),
        // ("list", "[1,2,3, abc, ]"),
        // ("mapexpr", "{1 + a: 3}"),
        // ("complex", "Account{user_id: 123}.user_id == 123"),
        // ("benchmark", "((TestDouble >= 1.0 || TestString.TestFunction() == 'HelloWorld') && (TestDouble + 1.0 >= 0.0)) || Now > TestTime"),
    ];
    // https://gist.github.com/rhnvrm/db4567fcd87b2cb8e997999e1366d406

    for (name, expr) in black_box(&expressions) {
        c.bench_function(name, |b| {
            let program = Program::compile(expr).expect("Parsing failed");
            b.iter(|| program.execute())
        });
        // c.bench_function(format!("{}-parsing", name).as_str(), |b| {
        //     b.iter(|| Program::compile(expr).expect("Parsing failed"))
        // });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
