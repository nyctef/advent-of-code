use std::collections::{HashMap, HashSet};

use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 19)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let (replacements, target) = input.trim().split_once("\n\n").unwrap();
    let replacements = replacements.lines().map(|l| {
        l.split_once(" => ").unwrap()
    }).collect_vec();

    let mut results = HashSet::new();

    for (source, dest) in replacements {
        for (i, _) in target.match_indices(source) {
            let mut new_string = target.to_string();
            new_string.replace_range(i..(i+source.len()), dest);
            results.insert(new_string);

        }

    }

    let part1 = results.len();
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
