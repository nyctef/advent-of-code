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

fn solve_for(input: &str) -> (u64, u64) {
    // ...
    let mut part1 = 0;
    let mut part2 = 0;

    todo!();
    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
    
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 0);
    assert_eq!(part2, 0);
}
