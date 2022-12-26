use std::error::Error;

use crate::aoc_util::get_input;

pub fn part1() -> Result<(), Box<dyn Error>> {
    let input = get_input(2019, 1)?;
    let sum = input
        .lines()
        .map(|x| get_mass(str::parse::<u32>(x).unwrap()))
        .sum::<u32>();

    print!("{sum}");
    Ok(())
}

fn get_mass(obj: u32) -> u32 {
    (obj / 3).saturating_sub(2)
}

#[test]
fn test1() {
    assert_eq!(2, get_mass(12));
    assert_eq!(0, get_mass(2));
}

pub fn part2() -> Result<(), Box<dyn Error>> {
    let _input = get_input(2019, 1)?;
    Ok(())
}
