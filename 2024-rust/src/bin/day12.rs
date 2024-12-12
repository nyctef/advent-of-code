use std::collections::VecDeque;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashSet;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 12)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let grid = CharGrid::from_string(input);

    let mut seen = FxHashSet::default();
    let mut part1 = 0;
    for (pos, plant) in grid.enumerate_chars_rc() {
        if seen.contains(&pos) {
            continue;
        }

        let mut flood_fill_queue = VecDeque::new();
        flood_fill_queue.push_back(pos);
        let mut perimeter = 0;
        let mut area = 0;
        while let Some(next) = flood_fill_queue.pop_front() {
            // eprintln!("{}: checking {}", plant, next);
            let already_seen = !seen.insert(next);
            if already_seen {
                continue;
            }
            area += 1;
            for neighbor in RCDirection::four().into_iter().map(|d| next + d) {
                // eprintln!("  : considering {}", neighbor);
                if grid.index_opt(neighbor) == Some(plant) {
                    flood_fill_queue.push_back(neighbor);
                } else {
                    perimeter += 1;
                }
            }
        }
        // dbg!(pos, plant, area, perimeter, &seen);
        part1 += area * perimeter;
    }

    let part2 = 0;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
AAAA
BBCD
BBCC
EEEC
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 140);
    assert_eq!(part2, 0);
    Ok(())
}
