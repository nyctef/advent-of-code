use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 1)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (usize, usize) {
    let digits = input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as usize)
        .collect_vec();

    let mut part1 = 0;
    let mut part2 = 0;
    for i in 0..digits.len() {
        if digits[i] == digits[(i + 1) % digits.len()] {
            part1 += digits[i];
        }

        if digits[i] == digits[(i + digits.len() / 2) % digits.len()] {
            part2 += digits[i];
        }
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
1122
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 3);
    assert_eq!(part2, 0);
}
