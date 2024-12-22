use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 22)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let seeds = input.trim().lines().map(|l| all_numbers_u64(l)[0]).collect_vec();

    let mut part1 = 0;

    for seed in seeds {
        let mut x = seed;
        for n in 0..2000 {
            x = next(x);
        }
        part1 += x;
    }

    let mut part2 = 0;

    (part1, part2)
}

fn next(mut x: u64) -> u64 {
    x = x ^ (x * 64);
    x = x % 16777216;
    x = x ^ (x / 32);
    x = x % 16777216;
    x = x ^ (x * 2048);
    x = x % 16777216;
    x
}

#[test]
fn test_example1() {
    let input = r###"
1
10
100
2024
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 37327623);
    assert_eq!(part2, 0);
}
