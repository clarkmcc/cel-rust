use cel_parser::{parse_bytes, parse_string};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn parse_string_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("string parsing");
    let expressions = vec![
        ("text", "\"text\""),
        ("raw", "r\"text\""),
        ("single unicode escape sequence", "\"\\U0001f431\""),
        ("single hex escape sequence", "\"\\x0D\""),
        ("single oct escape sequence", "\"\\015\""),
    ];

    for (name, expr) in black_box(expressions) {
        group.bench_function(name, |b| b.iter(|| parse_string(expr)));
    }
    group.finish()
}

pub fn parse_bytes_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("bytes parsing");
    let expressions = vec![
        ("bytes", "text"),
        ("single hex escape sequence", "x0D"),
        ("single oct escape sequence", "015"),
    ];

    for (name, expr) in black_box(expressions) {
        group.bench_function(name, |b| b.iter(|| parse_bytes(expr)));
    }
    group.finish()
}

criterion_group!(benches, parse_string_benchmark, parse_bytes_benchmark);
criterion_main!(benches);
