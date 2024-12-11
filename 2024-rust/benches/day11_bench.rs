use aoc_2024_rust::day11::blink_memoized_seq;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use rustc_hash::FxHashMap;
use std::collections::HashMap;

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("blink");
    for i in [25, 75, 1000].iter() {
        group.bench_with_input(BenchmarkId::new("HashMap", i), i, |b, i| {
            let mut cache = HashMap::new();
            b.iter(|| black_box(blink_memoized_seq(&mut cache, &[2024], *i)))
        });

        group.bench_with_input(BenchmarkId::new("FxHashMap", i), i, |b, i| {
            let mut cache = FxHashMap::default();
            b.iter(|| black_box(blink_memoized_seq(&mut cache, &[2024], *i)))
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
