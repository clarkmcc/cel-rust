use criterion::{
    criterion_group, criterion_main, Criterion, black_box
};
use cel_rust::cel;

pub fn criterion_benchmark(c: &mut Criterion) {
    let expressions = vec![
        ("call", "a.b(c + 3 / 2,) == 2"),
        ("list", "[1,2,3, abc, ]"),
        ("mapexpr", "{1 + a: 3}"),
        ("complex", "Account{user_id: 123}.user_id == 123"),
    ];

    let parser = cel::ExpressionParser::new();

    for (name, expr) in black_box(expressions) {
        c.bench_function(name, |b| b.iter(|| parser.parse(expr).unwrap_or_else(|e| panic!("{}", e))));
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
