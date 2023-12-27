use crate::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 24)?;

    let result = solve_for(&input, 200)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, time: usize) -> Result<String> {
    let starting_grid = NestedGrid::new(vec![CharGrid::from_string(input)], vec![None], vec![None]);

    let mut current_grid = starting_grid;
    for _ in 0..time {
        let mut new_grid = current_grid.clone();
        update_grid(&current_grid, &mut new_grid);
        current_grid = new_grid;
    }

    let num_bugs = current_grid
        .grids
        .iter()
        .map(|g| g.enumerate_chars_rc().filter(|&(_, c)| c == '#').count())
        .sum::<usize>();

    Ok(format!("num_bugs: {num_bugs}"))
}

fn update_grid(current_grid: &NestedGrid, new_grid: &mut NestedGrid) {
    new_grid.expand();
    for i in 0..current_grid.grids.len() {
        let cg = &current_grid.grids[i];
        let ng = &mut new_grid.grids[i];
        for (p, c) in cg.enumerate_chars_rc() {
            let n_count = current_grid.get_neighbor_count(i, p);
            ng.set_index_rc(
                p,
                if (c == '#' && n_count == 1) || (c == '.' && (n_count == 1 || n_count == 2)) {
                    '#'
                } else {
                    '.'
                },
            )
        }
    }
}

#[derive(Debug, Clone, Constructor)]
struct NestedGrid {
    grids: Vec<CharGrid>,
    outer: Vec<Option<usize>>,
    inner: Vec<Option<usize>>,
}

impl NestedGrid {
    pub fn expand(&mut self) {
        dbg!(&self);
        let outermost_grid = self
            .outer
            .iter()
            .enumerate()
            .filter(|(_i, x)| x.is_none())
            .map(|(i, _)| i)
            .exactly_one()
            .expect("exactly one outermost grid");
        let innermost_grid = self
            .inner
            .iter()
            .enumerate()
            .filter(|(_i, x)| x.is_none())
            .map(|(i, _)| i)
            .exactly_one()
            .expect("exactly one innermost grid");

        // if there are any bugs on the outer border of the outermost grid...
        let outermost_bugs = self.grids[outermost_grid]
            .enumerate_chars_rc()
            .filter(|&(_p, c)| c == '#')
            .filter(|&(p, _)| p.row == 0 || p.row == 4 || p.col == 0 || p.col == 4)
            .next()
            .is_some();

        if outermost_bugs {
            // ...then we need to make a new outermost grid
            let next_outer_grid_i = self.grids.len();
            self.grids.push(CharGrid::from_empty_char('.', 5, 5));
            // this new grid will have no outer grid yet
            self.outer.push(None);
            // but the new grid's inner grid will be the previous outermost grid
            self.inner.push(Some(outermost_grid));
            self.outer[outermost_grid] = Some(next_outer_grid_i);
        }

        // and same for the inner grid
        let innermost_bugs = self.grids[innermost_grid]
            .enumerate_chars_rc()
            .filter(|&(_p, c)| c == '#')
            .filter(|&(p, _)| p.row != 0 && p.row != 4 && p.col != 0 && p.col != 4)
            .next()
            .is_some();

        if innermost_bugs {
            let next_inner_grid_i = self.grids.len();
            self.grids.push(CharGrid::from_empty_char('.', 5, 5));
            // this new grid will have no more-inner grid yet
            self.inner.push(None);
            // but its outer grid will be the previous innermost grid
            self.outer.push(Some(innermost_grid));
            self.inner[innermost_grid] = Some(next_inner_grid_i);
        }
    }

    fn is_bug(&self, grid_index: Option<usize>, pos: CharGridIndexRC) -> usize {
        if let Some(grid_index) = grid_index {
            if self.grids[grid_index][pos] == '#' {
                1
            } else {
                0
            }
        } else {
            0
        }
    }

    pub fn get_neighbor_count(&self, grid_index: usize, pos: CharGridIndexRC) -> usize {
        // simple cases first
        let cg = &self.grids[grid_index];
        let mut n_count = RCDirection::four()
            .iter()
            .map(|d| pos + *d)
            .filter(|&p2| p2 != pos && cg.is_in_bounds(p2) && p2 != CharGridIndexRC::new(2, 2))
            .filter(|&p2| cg[p2] == '#')
            .count();

        /*

        0      1      2      3      4

        5      6      7      8      9

        10     11 a b c d e  13     14
                  f g h i j
                  k l m n o
                  p q r s t
                  u v w x y

        15     16     17     18     19

        20     21     22     23     24


        */
        let og = self.outer[grid_index];
        let ig = self.inner[grid_index];
        if pos.row == 0 {
            // a-e => 7
            n_count += self.is_bug(og, CharGridIndexRC::new(1, 2));
        }
        if pos.row == 4 {
            // u-y => 17
            n_count += self.is_bug(og, CharGridIndexRC::new(3, 2));
        }
        if pos.col == 0 {
            // a, f, k, p, u => 11
            n_count += self.is_bug(og, CharGridIndexRC::new(2, 1));
        }
        if pos.col == 4 {
            // e, j, o, t, y => 13
            n_count += self.is_bug(og, CharGridIndexRC::new(2, 3));
        }

        if pos == CharGridIndexRC::new(2, 3) {
            // 13 => e, j, o, t, y
            for row in 0..5 {
                n_count += self.is_bug(ig, CharGridIndexRC::new(row, 4));
            }
        }
        if pos == CharGridIndexRC::new(3, 2) {
            // 17
            for col in 0..5 {
                n_count += self.is_bug(ig, CharGridIndexRC::new(4, col));
            }
        }
        if pos == CharGridIndexRC::new(3, 2) {
            // 11
            for row in 0..5 {
                n_count += self.is_bug(ig, CharGridIndexRC::new(row, 0));
            }
        }
        if pos == CharGridIndexRC::new(1, 2) {
            // 7
            for col in 0..5 {
                n_count += self.is_bug(ig, CharGridIndexRC::new(0, col));
            }
        }

        n_count
    }
}

/*
fn rate_grid(grid: &CharGrid) -> usize {
    let mut p = 1;
    let mut total = 0;
    for (_, c) in grid.enumerate_chars_rc() {
        if c == '#' {
            total += p;
        }

        p *= 2;
    }
    total
}
*/

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
....#
#..#.
#..##
..#..
#.... 
"###;
    let result = solve_for(input, 10)?;

    assert_eq!("num_bugs: 99", result);
    Ok(())
}
