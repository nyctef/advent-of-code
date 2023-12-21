use std::collections::HashSet;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 21)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);

    let mut start = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| c == &'S')
        .exactly_one()
        .map(|(p, _c)| p)
        .unwrap();
    let mut starting_set = HashSet::new();
    starting_set.insert(start);
    let mut next_step = HashSet::new();
    for _ in 0..64 {
        for p in &starting_set {
            for dir in RCDirection::four() {
                let p2 = *p + dir;
                if p2 == *p || !grid.is_in_bounds(p2) {
                    continue;
                }
                if grid[p2] == '#' {
                    continue;
                }
                next_step.insert(p2);
            }

        }

        starting_set.clear();
        starting_set.extend(&next_step);
        next_step.clear();
    }

    let part1 = starting_set.len();
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
