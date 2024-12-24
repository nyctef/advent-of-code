use std::time::Instant;

use aoc_2024_rust::util::*;
use color_eyre::eyre::{eyre, Result};
use itertools::Itertools;
use rustc_hash::FxHashSet;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 6)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let grid = CharGrid::from_string(input);
    let (guard_original_pos, _) = grid
        .enumerate_chars_rc()
        .filter(|&(_, c)| c == '^')
        .exactly_one()
        .map_err(|_| eyre!("can't find guard position"))?;

    let mut guard_pos = guard_original_pos;
    let mut guard_direction = RCDirection::up();
    let mut guard_visited_positions = FxHashSet::default();

    let part1_timer = Instant::now();

    while grid.is_in_bounds(guard_pos) {
        guard_visited_positions.insert(guard_pos);
        let mut next_pos;
        loop {
            next_pos = guard_pos + guard_direction;
            if grid.index_opt(next_pos) == Some('#') {
                guard_direction = guard_direction.clockwise();
            } else {
                break;
            }
        }
        guard_pos = next_pos;
    }

    eprintln!("part 1 took {}ms", part1_timer.elapsed().as_millis());
    eprintln!("visited positions: {}", guard_visited_positions.len());
    let part2_timer = Instant::now();

    let mut part2 = 0;
    for &possible_obstacle in guard_visited_positions.iter() {
        if does_guard_loop(&grid, possible_obstacle) && possible_obstacle != guard_original_pos {
            part2 += 1;
        }
    }
    eprintln!("part 2 took {}ms", part2_timer.elapsed().as_millis());

    Ok((guard_visited_positions.len().try_into().unwrap(), part2))
}

fn does_guard_loop(grid: &CharGrid, extra_obstacle: CharGridIndexRC) -> bool {
    let (guard_original_pos, _) = grid
        .enumerate_chars_rc()
        .filter(|&(_, c)| c == '^')
        .exactly_one()
        .unwrap();

    let mut guard_pos = guard_original_pos;
    let mut guard_direction = RCDirection::up();
    let mut guard_visited_positions_directions = FxHashSet::default();

    while grid.is_in_bounds(guard_pos) {
        let is_new_position =
            guard_visited_positions_directions.insert((guard_pos, guard_direction));
        if !is_new_position {
            // we've looped
            return true;
        }
        let mut next_pos;
        loop {
            next_pos = guard_pos + guard_direction;
            if grid.index_opt(next_pos) == Some('#') || next_pos == extra_obstacle {
                guard_direction = guard_direction.clockwise();
            } else {
                break;
            }
        }
        guard_pos = next_pos;
    }
    // escaped a loop
    false
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 41);
    assert_eq!(part2, 6);
    Ok(())
}
