use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    ops::Add,
};

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

    let mut central_grid_seen_states = HashMap::new();

    for step in 0..step_count {
        let points_inside_original_grid = starting_set
            .iter()
            .filter(|p| {
                p.row >= 0
                    && p.row < grid.height() as isize
                    && p.col >= 0
                    && p.col < grid.width() as isize
            })
            .sorted_by_key(|p| (p.row, p.col))
            .copied()
            .collect_vec();
        let maybe_prev_step = central_grid_seen_states
            .entry(points_inside_original_grid.clone())
            .or_insert(step);
        // if *maybe_prev_step != step {
        //     println!("found central grid repitition: step {} is same as step {}", step, maybe_prev_step);
        // }
        if *maybe_prev_step == step {
            println!("found new central grid state at step {} : {:?}", step, points_inside_original_grid)
        }

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

#[derive(Clone, Copy, PartialEq, Eq, Constructor, Hash)]
pub struct WrappingIndexRC {
    pub row: isize,
    pub col: isize,
}
impl std::fmt::Debug for WrappingIndexRC {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("WI({},{})", self.row, self.col))
    }
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
