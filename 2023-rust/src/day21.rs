use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::{collections::HashSet, ops::Add};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 21)?;

    let result = solve_for(&input, 26501365)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, step_count: usize) -> Result<String> {
    let grid = CharGrid::from_string(input);

    let start = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| c == &'S')
        .exactly_one()
        .map(|(p, _c)| p)
        .unwrap();
    let start = WrappingIndexRC::new(start.row as isize, start.col as isize);
    let mut starting_set = HashSet::new();
    starting_set.insert(start);
    let mut next_step = HashSet::new();
    for _ in 0..step_count {
        for p in &starting_set {
            for dir in RCDirection::four() {
                let p2 = *p + dir;
                if grid[p2.wrap(grid.width(), grid.height())] == '#' {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Constructor, Hash)]
pub struct WrappingIndexRC {
    pub row: isize,
    pub col: isize,
}

impl WrappingIndexRC {
    fn wrap(&self, width: usize, height: usize) -> CharGridIndexRC {
        CharGridIndexRC::new(
            self.row.rem_euclid(width as isize) as usize,
            self.col.rem_euclid(height as isize) as usize,
        )
    }
}

impl Add<RCDirection> for WrappingIndexRC {
    type Output = WrappingIndexRC;

    fn add(self, rhs: RCDirection) -> Self::Output {
        Self::Output {
            row: self.row + rhs.rowdiff,
            col: self.col + rhs.coldiff,
        }
    }
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
    assert_eq!("total: 1594", solve_for(input, 50)?);
    assert_eq!("total: 167004", solve_for(input, 500)?);
    // assert_eq!("total: 16733044", solve_for(input, 5000)?);
    Ok(())
}
