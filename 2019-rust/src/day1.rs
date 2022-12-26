use std::error::Error;

use crate::aoc_util::get_input;

pub fn solve() -> Result<(), Box<dyn Error>> {
    let input = get_input(2019, 1)?;
    let sum = input
        .lines()
        .map(|x| get_mass(str::parse::<u32>(x).unwrap()))
        .sum::<u32>();

    print!("{sum}");
    Ok(())
}

fn get_mass(obj: u32) -> u32 {
    let mut total = 0;
    let mut next = obj;
    while next != 0 {
        let fuel_required = (next / 3).saturating_sub(2);
        total += fuel_required;
        next = fuel_required;
    }
    return total;
}

#[test]
fn test1() {
    assert_eq!(966, get_mass(1969));
    assert_eq!(2, get_mass(12));
    assert_eq!(0, get_mass(2));
}
