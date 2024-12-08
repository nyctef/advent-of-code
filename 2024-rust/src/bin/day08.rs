use std::collections::HashSet;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 8)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, usize)> {
    let grid = CharGrid::from_string(input);

    let mut antennas = FxHashMap::default();

    for (pos, char) in grid.enumerate_chars_rc() {
        if char == '.' {
            continue;
        }

        antennas.entry(char).or_insert(Vec::new()).push(pos);
    }

    let mut nodes = HashSet::new();
    for (_, ant) in antennas.iter() {
        for (a1, a2) in ant.iter().tuple_combinations() {
            let dir = RCDirection::from_to(a1, a2);
            let node1 = *a2 + dir;
            let node2 = *a1 - dir;
            if grid.is_in_bounds(node1) {
                nodes.insert(node1);
            }
            if grid.is_in_bounds(node2) {
                nodes.insert(node2);
            }
        }
    }
    let part1 = nodes.len();

    nodes.clear();

    for (_, ant) in antennas.iter() {
        for (&a1, &a2) in ant.iter().tuple_combinations() {
            let dir = RCDirection::from_to(&a1, &a2);

            nodes.insert(a1);
            nodes.insert(a2);

            let mut a = a1;
            while grid.is_in_bounds(a) {
                nodes.insert(a);
                a = a - dir;
            }
            let mut a = a2;
            while grid.is_in_bounds(a) {
                nodes.insert(a);
                a = a + dir;
            }

        }

    }
    let part2 = nodes.len();
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 14);
    assert_eq!(part2, 34);
    Ok(())
}
