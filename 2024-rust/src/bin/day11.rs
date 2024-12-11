use aoc_2024_rust::util::*;
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

fn blink_memoized_seq<T: std::hash::BuildHasher>(
    cache: &mut HashMap<(usize, u64), usize, T>,
    sequence: &[u64],
    target_num_steps: usize,
) -> usize {
    let mut result = 0;
    for &stone in sequence {
        result += blink_memoized_stone(cache, stone, target_num_steps);
    }
    result
}

fn blink_memoized_stone<T: std::hash::BuildHasher>(
    cache: &mut HashMap<(usize, u64), usize, T>,
    stone: u64,
    target_num_steps: usize,
) -> usize {
    if target_num_steps == 0 {
        return 1;
    }
    if let Some(&entry) = cache.get(&(target_num_steps, stone)) {
        entry
    } else {
        let sequence = blink(stone);
        let resulting_length = sequence
            .into_iter()
            .map(|s| blink_memoized_stone(cache, s, target_num_steps - 1))
            .sum();
        cache.insert((target_num_steps, stone), resulting_length);
        resulting_length
    }
}

fn blink(stone: u64) -> Vec<u64> {
    if stone == 0 {
        return vec![1];
    } else if stone.ilog10() % 2 == 1 {
        // even number of digits
        let str = stone.to_string();
        let left = &str[0..str.len() / 2];
        let right = &str[str.len() / 2..str.len()];
        return vec![left.parse().unwrap(), right.parse().unwrap()];
    } else {
        return vec![stone * 2024];
    }
}

#[test]
fn test_example1() -> Result<()> {
    let sequence = vec![0, 1, 10, 99, 999];

    let result = sequence.into_iter().map(blink).collect_vec();

    assert_eq!(
        result,
        vec![vec![1], vec![2024], vec![1, 0], vec![9, 9], vec![2021976]]
    );
    Ok(())
}
