use std::collections::HashMap;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 14)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut grid = CharGrid::from_string(input);

    let mut seen_grids = HashMap::new();

    let mut c = 0;
    let mut found_loop = false;
    while c < 1_000_000_000 {
        println!("c: {} load: {}", c, calc_north_load(&grid));
        let x = seen_grids.entry(grid.clone()).or_insert(c);
        if *x != c && !found_loop {
            println!("at time c: {} found repeated state from time x: {}", c, x);
            let loop_length = c - *x;

            while (c + loop_length) < 1_000_000_000 {
                c += loop_length;
            }
            println!("jumped to c: {}", c);
            found_loop = true;
        }
        cycle(&mut grid);
        c += 1;
    }

    dbg!(&grid);

    let load = calc_north_load(&grid);

    Ok(format!("load: {load}"))
}

fn calc_north_load(grid: &CharGrid) -> usize{
    let mut load = 0;

    for (final_pos, _) in grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == 'O')
        .collect_vec()
    {
        load += grid.height() - final_pos.row;
    }

    load
}

fn cycle(grid: &mut CharGrid) {
    let rollers = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == 'O')
        .collect_vec();

    for (p, _) in rollers {
        tilt(grid, p, |p| p.up());
    }

    let rollers = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == 'O')
        .map(|(p, _)| p)
        .sorted_by_key(|p| p.col)
        .collect_vec();
    for p in rollers {
        tilt(grid, p, |p| p.left());
    }
    let rollers = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == 'O')
        .map(|(p, _)| p)
        .sorted_by_key(|p| -(p.row as isize))
        .collect_vec();
    let height = grid.height();
    for p in rollers {
        tilt(grid, p, |p| {
            if p.down().row < height {
                Some(p.down())
            } else {
                None
            }
        });
    }
    let rollers = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == 'O')
        .map(|(p, _)| p)
        .sorted_by_key(|p| -(p.col as isize))
        .collect_vec();
    let width = grid.width();
    for p in rollers {
        tilt(grid, p, |p| {
            if p.right().col < width {
                Some(p.right())
            } else {
                None
            }
        });
    }
}

fn tilt(
    grid: &mut CharGrid,
    p: CharGridIndexRC,
    dir: impl Fn(CharGridIndexRC) -> Option<CharGridIndexRC>,
) {
    // pick up the rock
    grid.set_index_rc(p, '.');
    let mut current_pos = p;

    // figure out if it can move
    loop {
        let next_pos = dir(current_pos);
        if next_pos.is_none() {
            break;
        }
        if grid.index(next_pos.unwrap()) != '.' {
            break;
        }
        current_pos = next_pos.unwrap();
    }
    // put it down again
    grid.set_index_rc(current_pos, 'O');
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
"###;
    let result = solve_for(input)?;

    assert_eq!("load: 64", result);
    Ok(())
}
