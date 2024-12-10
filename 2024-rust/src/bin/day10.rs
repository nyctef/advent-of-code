use std::collections::{HashSet, VecDeque};

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 10)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, u64)> {
    let grid = CharGrid::from_string(input);

    let mut part1 = 0;
    for (pos, char) in grid.enumerate_chars_rc() {
        let mut reachable_nines = HashSet::new();
        if char != '0' {
            continue;
        }

        let mut trail = VecDeque::new();
        trail.push_back(pos);
        while let Some(next) = trail.pop_back() {
            let current_height = grid[next];
            if current_height == '9' {
                reachable_nines.insert(next);
                continue;
            }

            for neighbor in RCDirection::four().into_iter().map(|d| next + d) {
                if grid.index_opt(neighbor) == Some((current_height as u8 + 1) as char) {
                    trail.push_back(neighbor);
                }
            }
        }

        // if !reachable_nines.is_empty() {
        //     eprintln!(
        //         "found {} nines reachable from {}",
        //         reachable_nines.len(),
        //         pos
        //     );
        // }
        part1 += reachable_nines.len();
    }
    let part2 = 0;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 36);
    assert_eq!(part2, 0);
    Ok(())
}
