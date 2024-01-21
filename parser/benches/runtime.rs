use cel_parser::parse_string;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn parse_string_benchmark(c: &mut Criterion) {
    let expressions = vec![
        ("text", "\"text\""),
        ("raw", "r\"text\""),
        ("single unicode escape sequence", "\"\\U0001f431\""),
        ("single hex escape sequence", "\"\\x0D\""),
        ("single oct escape sequence", "\"\\015\""),
    ];

    for (name, expr) in black_box(&expressions) {
        c.bench_function(name, |b| b.iter(|| parse_string(expr)));
    }
}

criterion_group!(benches, parse_string_benchmark);
criterion_main!(benches);
