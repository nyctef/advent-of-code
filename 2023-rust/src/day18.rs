use std::str::FromStr;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use regex::Regex;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 18)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let re = Regex::from_str(r"^(.) (\d+) \((.*)\)$")?;
    let lines = input.trim().lines().map(|l| re.captures(l).unwrap()).map(|c| (c[1].to_string(), c[2].parse::<u32>().unwrap())).collect_vec();

    dbg!(&lines);

    todo!();
    let part1 = "";
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: ", result);
    Ok(())
}
