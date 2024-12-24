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
        move_guard(
            &grid,
            CharGridIndexRC::new(grid.width() + 1, grid.height() + 1),
            &mut guard_pos,
            &mut guard_direction,
        );
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
    let guard_original_pos = grid.find_single_char('^');

    // https://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare

    let mut tortoise_pos = guard_original_pos;
    let mut tortoise_dir = RCDirection::up();
    let mut hare_pos = guard_original_pos;
    let mut hare_dir = RCDirection::up();

    while grid.is_in_bounds(tortoise_pos) && grid.is_in_bounds(hare_pos) {
        move_guard(grid, extra_obstacle, &mut tortoise_pos, &mut tortoise_dir);
        move_guard(grid, extra_obstacle, &mut hare_pos, &mut hare_dir);
        move_guard(grid, extra_obstacle, &mut hare_pos, &mut hare_dir);

        if hare_pos == tortoise_pos && hare_dir == tortoise_dir {
            return true;
        }
    }
    // escaped a loop
    false
}

fn move_guard(
    grid: &CharGrid,
    extra_obstacle: CharGridIndexRC,
    guard_pos: &mut CharGridIndexRC,
    guard_direction: &mut RCDirection,
) {
    let mut next_pos;
    loop {
        next_pos = *guard_pos + *guard_direction;
        if grid.index_opt(next_pos) == Some('#') || next_pos == extra_obstacle {
            *guard_direction = guard_direction.clockwise();
        } else {
            break;
        }
    }
    *guard_pos = next_pos;
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
