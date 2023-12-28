use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    let input = get_input(2015, __DAY__)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    // ...

    todo!();
    let part1 = "";
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: ", result);
    Ok(())
}
