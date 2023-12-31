use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 10)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut sequence = input.trim().to_owned();
    for _ in 0..40 {
        sequence = step(&sequence);
    }

    let part1 = sequence.len();

    for _ in 0..10 {
        sequence = step(&sequence)
    }
    let part2 = sequence.len();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn step(input: &str) -> String {
    let mut new_sequence = String::new();
    for (count, char) in input.chars().dedup_with_count() {
        new_sequence.push(char::from_digit(count as u32, 10).unwrap());
        new_sequence.push(char);
    }
    new_sequence
}

#[test]
fn test_example1() {
    assert_eq!(step("1"), "11");
    assert_eq!(step("11"), "21");
    assert_eq!(step("21"), "1211");
    assert_eq!(step("1211"), "111221");
    assert_eq!(step("111221"), "312211");
}
