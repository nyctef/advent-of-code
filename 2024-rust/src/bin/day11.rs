use std::collections::{hash_map, HashMap};

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 11)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, usize)> {
    let mut sequence = all_numbers_u64(input.trim());

    let mut cache = HashMap::new();
    let part1 = blink_memoized_seq(&mut cache, &sequence, 25);
    let part2 = blink_memoized_seq(&mut cache, &sequence, 75);

    Ok((part1, part2))
}

fn blink_memoized_seq(
    cache: &mut HashMap<(usize, u64), usize>,
    sequence: &[u64],
    target_num_steps: usize,
) -> usize {
    let mut result = 0;
    for &stone in sequence {
        result += blink_memoized_stone(cache, stone, target_num_steps);
    }
    result
}

fn blink_memoized_stone(
    cache: &mut HashMap<(usize, u64), usize>,
    stone: u64,
    target_num_steps: usize,
) -> usize {
    if target_num_steps == 0 {
        return 1;
    }
    if let Some(&entry) = cache.get(&(target_num_steps, stone)) {
        return entry;
    } else {
        let mut sequence = vec![stone];
        blink(&mut sequence);
        let resulting_length = sequence
            .into_iter()
            .map(|s| blink_memoized_stone(cache, s, target_num_steps - 1))
            .sum();
        cache.insert((target_num_steps, stone), resulting_length);
        return resulting_length;
    }
}

fn blink(sequence: &mut Vec<u64>) {
    let mut i = 0;
    while i < sequence.len() {
        let stone = sequence[i];
        if stone == 0 {
            sequence[i] = 1;
            i += 1;
        } else if stone.ilog10() % 2 == 1 {
            // even number of digits
            let str = stone.to_string();
            let left = &str[0..str.len() / 2];
            let right = &str[str.len() / 2..str.len()];
            let to_insert = [left.parse().unwrap(), right.parse().unwrap()];
            sequence.splice(i..i + 1, to_insert);
            i += 2;
        } else {
            sequence[i] *= 2024;
            i += 1;
        }
    }
}

#[test]
fn test_example1() -> Result<()> {
    let mut sequence = vec![0, 1, 10, 99, 999];
    blink(&mut sequence);

    assert_eq!(sequence, vec![1, 2024, 1, 0, 9, 9, 2021976]);
    Ok(())
}
