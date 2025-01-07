use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 15)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let mut part1 = 0;
    let mut part2 = 0;

    let mut nums = input
        .trim()
        .lines()
        .map(|l| all_numbers_u64(l)[0])
        .collect_vec();

    let mut gen1 = nums[0];
    let mut gen2 = nums[1];

    let mask = u16::MAX as u64;

    for _ in 0..40_000_000 {
        gen1 *= 16807;
        gen1 %= 2147483647;
        gen2 *= 48271;
        gen2 %= 2147483647;

        if gen1 & mask == gen2 & mask {
            part1 += 1;
        }
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
Generator A starts with 65
Generator B starts with 8921
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 588);
    assert_eq!(part2, 0);
}
