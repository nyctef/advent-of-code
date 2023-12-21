use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use num::traits::Euclid;
use std::{
    collections::{HashMap, HashSet},
    ops::Add,
    task::Wake,
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
    let mut starting_set = HashSet::new();
    starting_set.insert(GGIndexRC::new(0, 0, start.row, start.col));
    let mut next_step = HashSet::new();
    let mut final_total: usize = 0;
    let parity = step_count % 2;

    let mut central_grid_seen_states = HashMap::new();

    for step in 0..step_count {
        let mut points_by_grid: HashMap<(isize, isize), Vec<GGIndexRC>> = HashMap::new();

        for (key, group) in &starting_set
            .iter()
            // .map(|p| {
            //     (
            //         (
            //             div_floor(p.row, grid.height() as isize),
            //             div_floor(p.col, grid.width() as isize),
            //         ),
            //         p.wrap(grid.width(), grid.height()),
            //     )
            // })
            .group_by(|gp| (gp.x, gp.y))
        {
            let collection = points_by_grid.entry(key).or_default();
            collection.extend(group);
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

        let this_step_parity = step % 2;
        if *maybe_prev_step != step && this_step_parity == parity {
            println!(
                "found repeated central grid state at step {} : {:?}",
                step, points_inside_original_grid
            );
            final_total += points_inside_original_grid.len();
            starting_set.retain(|p| !points_inside_original_grid.contains(p));
        }

        for p in &starting_set {
            for p2 in [p.up(&grid), p.right(&grid), p.down(&grid), p.left(&grid)] {
                if grid.index_rc(p2.row, p2.col) == '#' {
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
