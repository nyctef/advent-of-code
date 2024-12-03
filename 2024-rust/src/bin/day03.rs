use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use regex::Regex;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 3)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let input = input.trim();

    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();

    let mut sum:u64 = 0;

    for (_, [x, y]) in re.captures_iter(input).map(|c| c.extract()) {
        sum += x.parse::<u64>().unwrap() * y.parse::<u64>().unwrap();
    }

    let part1 = sum;
    let part2 = 0;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 161);
    assert_eq!(part2, 0);
    Ok(())
}
