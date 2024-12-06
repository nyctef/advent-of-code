use std::collections::HashSet;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 6)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let grid = CharGrid::from_string(input);
    // TODO: why doesn't this compile?
    // let (guard_pos, _) = grid.enumerate_chars_rc().filter(|(pos, c): &(CharGridIndexRC, char)| *c == '^').exactly_one()?;
    let mut guard_pos = CharGridIndexRC::zero();
    for (pos, char) in grid.enumerate_chars_rc() {
        if char == '^' {
            guard_pos = pos;
            break;
        }
    }

    let mut guard_direction = RCDirection::up();
    let mut guard_visited_positions = HashSet::new();

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

    let part2 = 0;
    Ok((guard_visited_positions.len().try_into().unwrap(), part2))
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
    assert_eq!(part2, 0);
    Ok(())
}
