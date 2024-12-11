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

fn solve_for(input: &str) -> Result<(usize, u64)> {
    let mut sequence = all_numbers_u64(input.trim());

    for i in 0..25 {
        blink(&mut sequence);
    }

    let part1 = sequence.len();
    let part2 = 0;
    Ok((part1, part2))
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
