use std::error::Error;

use crate::aoc_util::get_input;

pub fn part1() -> Result<(), Box<dyn Error>> {
    let input = get_input(2019, 1)?;
    print!("{input}");
    Ok(())
}

pub fn part2() -> Result<(), Box<dyn Error>> {
    let input = get_input(2019, 1)?;
    Ok(())
}
