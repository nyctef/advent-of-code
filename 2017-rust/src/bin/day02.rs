use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 2)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let sheet = input.trim().lines().map(all_numbers_u64).collect_vec();

    let part1 = sheet
        .iter()
        .map(|l| l.iter().max().unwrap() - l.iter().min().unwrap())
        .sum();
    let mut part2 = 0;

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
5 1 9 5
7 5 3
2 4 6 8
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 18);
    assert_eq!(part2, 0);
}
