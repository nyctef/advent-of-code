use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use num::traits::Euclid;
use rustc_hash::FxHasher;
use std::collections::BTreeSet;
use std::hash::BuildHasherDefault;
use std::io;
use std::io::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    ops::Add,
    task::Wake,
};
use unordered_hash::UnorderedHasher;

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
    let mut starting_set: HashSet<GGIndexRC, BuildHasherDefault<FxHasher>> = Default::default();
    starting_set.insert(GGIndexRC::new(0, 0, start.row, start.col));
    let mut next_step: HashSet<GGIndexRC, BuildHasherDefault<FxHasher>> = Default::default();
    let mut grid_is_cycling: HashMap<(isize, isize), bool, BuildHasherDefault<FxHasher>> =
        Default::default();
    let mut final_total: usize = 0;
    let mut frozen_grids: HashMap<(isize, isize), bool, BuildHasherDefault<FxHasher>> =
        Default::default();
    // since grids cycle with a period of 2, we want to make sure we end up
    // calculating results for the correct part of the period.
    // all the examples end on an even number of steps, but the actual
    // puzzle ends on an odd step, so hopefully this means we do the
    // right thing there
    let parity = step_count % 2;

    let mut grid_seen_states: HashMap<Vec<GGIndexRC>, usize, BuildHasherDefault<FxHasher>> =
        Default::default();

    for step in 0..step_count {
        // print!(".");
        // io::stdout().flush().ok().expect("Could not flush stdout");
        let mut points_by_grid: HashMap<
            (isize, isize),
            Vec<GGIndexRC>,
            BuildHasherDefault<FxHasher>,
        > = Default::default();

        for (key, group) in &starting_set.iter().group_by(|gp| (gp.x, gp.y)) {
            let collection = points_by_grid.entry(key).or_default();
            collection.extend(group);
        }

        // TODO: we should really be sorting the per-grid points now to make
        // sure they hash consistently, but that turns out to be expensive.
        // maybe we can get UnorderedHasher to work with a custom Hasher impl
        // which is supported for iterators only?

        for (g, points_inside_grid) in points_by_grid {
            let maybe_prev_step = grid_seen_states
                .entry(points_inside_grid.clone())
                .or_insert(step);

            let this_step_parity = step % 2;
            if *maybe_prev_step != step && this_step_parity == parity {
                // println!(
                //     "found repeated grid {:?} state at step {} : {:?}",
                //     g,
                //     step, points_inside_grid
                // );
                // dbg!(&grid_is_cycling);
                grid_is_cycling.insert(g, true);
                if [
                    (g.0 - 1, g.1),
                    (g.0, g.1 - 1),
                    (g.0 + 1, g.1),
                    (g.0, g.1 + 1),
                ]
                .iter()
                .all(|g2| *grid_is_cycling.entry(*g2).or_insert(false))
                {
                    final_total += points_inside_grid.len();
                    starting_set.retain(|p| !points_inside_grid.contains(p));
                    frozen_grids.insert(g, true);
                    println!(
                        "step {} : froze grid {:?} but {} points remain",
                        step,
                        g,
                        starting_set.len()
                    );
                }
            }
        }

        for p in &starting_set {
            for p2 in [p.up(&grid), p.right(&grid), p.down(&grid), p.left(&grid)] {
                if grid.index_rc(p2.row, p2.col) == '#' {
                    continue;
                }
                if frozen_grids.contains_key(&(p2.x, p2.y)) {
                    continue;
                }
                next_step.insert(p2);
            }
        }

        starting_set.clear();
        starting_set.extend(&next_step);
        next_step.clear();
    }
    final_total += starting_set.len();

    Ok(format!("total: {}", final_total))
}

#[derive(Clone, Copy, PartialEq, Eq, Constructor, Hash, PartialOrd, Ord)]
pub struct GGIndexRC {
    // which grid we're on
    pub x: isize,
    // we'll make positive y downwards just so it's the same direction as rows
    pub y: isize,
    // row/col within that grid
    pub row: usize,
    pub col: usize,
}

impl GGIndexRC {
    pub fn up(&self, grid: &CharGrid) -> Self {
        if self.row == 0 {
            Self::new(self.x, self.y - 1, grid.height() - 1, self.col)
        } else {
            Self::new(self.x, self.y, self.row - 1, self.col)
        }
    }
    pub fn down(&self, grid: &CharGrid) -> Self {
        if self.row == grid.height() - 1 {
            Self::new(self.x, self.y + 1, 0, self.col)
        } else {
            Self::new(self.x, self.y, self.row + 1, self.col)
        }
    }
    pub fn right(&self, grid: &CharGrid) -> Self {
        if self.col == grid.width() - 1 {
            Self::new(self.x + 1, self.y, self.row, 0)
        } else {
            Self::new(self.x, self.y, self.row, self.col + 1)
        }
    }
    pub fn left(&self, grid: &CharGrid) -> Self {
        if self.col == 0 {
            Self::new(self.x - 1, self.y, self.row, grid.width() - 1)
        } else {
            Self::new(self.x, self.y, self.row, self.col - 1)
        }
    }
}

impl std::fmt::Debug for GGIndexRC {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "GG(gx{} gy{} r{} c{})",
            self.x, self.y, self.row, self.col
        ))
    }
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

    // assert_eq!("total: 16", solve_for(input, 6)?);
    // assert_eq!("total: 50", solve_for(input, 10)?);
    assert_eq!("total: 1594", solve_for(input, 50)?);
    // assert_eq!("total: 167004", solve_for(input, 500)?);
    // assert_eq!("total: 668697", solve_for(input, 1000)?);
    assert_eq!("total: 16733044", solve_for(input, 5000)?);
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
