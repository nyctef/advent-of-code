use aoc_2024_rust::{day11::blink_memoized_seq, util::*};
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::collections::HashMap;
// use rustc_hash::FxHashMap as HashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 11)?;

    let (part1, part2, thousand) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {} | 1000: {}", part1, part2, thousand);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, usize, usize)> {
    let sequence = all_numbers_u64(input.trim());

    let mut cache = FxHashMap::default();
    let part1 = blink_memoized_seq(&mut cache, &sequence, 25);
    let part2 = blink_memoized_seq(&mut cache, &sequence, 75);
    let thousand = blink_memoized_seq(&mut cache, &sequence, 1000);

    Ok((part1, part2, thousand))
}
