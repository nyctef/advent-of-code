use std::collections::HashSet;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 21)?;

    let result = solve_for(&input,26501365 )?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, step_count: usize) -> Result<String> {
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
    for _ in 0..step_count {
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

    Ok(format!("total: {}", starting_set.len()))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"###;

    assert_eq!("total: 16", solve_for(input, 6)?);
    assert_eq!("total: 50", solve_for(input, 10)?);
    Ok(())
}
