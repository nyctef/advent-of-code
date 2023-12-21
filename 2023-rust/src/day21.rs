use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use num::traits::Euclid;
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
    let mut final_total: u64 = 0;
    let parity = step_count % 2;
    
    // TODO: something like: if we see a grid go into a cycle, and the current step count is
    // even or odd according to the final step count, then remove all the points from that grid
    // and add that grid's current count of points to the final total

    let mut central_grid_seen_states = HashMap::new();

    for step in 0..step_count {
        let mut points_by_grid: HashMap<(isize, isize), Vec<CharGridIndexRC>> = HashMap::new();

        for (key, group) in &starting_set
            .iter()
            .map(|p| {
                (
                    (
                        div_floor(p.row, grid.height() as isize),
                        div_floor(p.col, grid.width() as isize),
                    ),
                    p.wrap(grid.width(), grid.height()),
                )
            })
            .group_by(|gp| gp.0)
        {
            let collection = points_by_grid.entry(key).or_default();
            collection.extend(group.map(|gp| gp.1));
            // todo: this repeated sorting is probably slow
            collection.sort_by_key(|p| (p.row, p.col));

        }
        let points_inside_original_grid = &points_by_grid[&(0, 0)];
        // if !(points_inside_original_grid == by_division) {
        //     assert_eq!(points_inside_original_grid, by_division);
        //     panic!(
        //         "points inside: {:?} by_division: {:?}",
        //         points_inside_original_grid, by_division
        //     );
        // }
        let maybe_prev_step = central_grid_seen_states
            .entry(points_inside_original_grid.clone())
            .or_insert(step);
        // if *maybe_prev_step != step {
        //     println!("found central grid repitition: step {} is same as step {}", step, maybe_prev_step);
        // }
        if *maybe_prev_step == step {
            println!(
                "found new central grid state at step {} : {:?}",
                step, points_inside_original_grid
            )
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

// since the proper impl for this is still nightly
pub const fn div_floor(lhs: isize, rhs: isize) -> isize {
    let d = lhs / rhs;
    let r = lhs % rhs;
    if (r > 0 && rhs < 0) || (r < 0 && rhs > 0) {
        d - 1
    } else {
        d
    }
}

#[test]
fn test_div() {
    assert_eq!(div_floor(-1, 10), -1);
    assert_eq!(div_floor(1, 10), 0);
    assert_eq!(div_floor(11, 10), 1);
}
