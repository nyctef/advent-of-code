use std::collections::VecDeque;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 16)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let grid = CharGrid::from_string(input);
    let mut best_score_found = u64::MAX;

    let mut seen = FxHashMap::default();
    let mut queue = VecDeque::new();
    let start = grid.find_single_char('S');
    let end = grid.find_single_char('E');
    queue.push_back((0, start, RCDirection::right()));

    while let Some((score, pos, dir)) = queue.pop_front() {
        if pos.col == 13 {
        // eprintln!("s {} p {} d {}", score, pos, dir);
        }
        if score > best_score_found {
            continue;
        }
        if score + RCDirection::from_to(pos, end).manhattan_abs() as u64 > best_score_found {
            continue;
        }
        let score_at_point = seen.entry((pos, dir)).or_insert(score);
        if *score_at_point < score {
            continue;
        }
        *score_at_point = score;
        if pos == end {
            // eprintln!("found a path to end with score {}", score);
            best_score_found = score;
            continue;
        }

        let char_in_front = grid[pos + dir];
        if char_in_front == '.' || char_in_front == 'E' {
            queue.push_front((score + 1, pos + dir, dir));
        }

        let dir_left = dir.counterclockwise();
        if grid[pos + dir_left] == '.' {
            queue.push_front((score + 1000, pos, dir_left));
        }
        let dir_right = dir.clockwise();
        if grid[pos + dir_right] == '.' {
            queue.push_front((score + 1000, pos, dir_right));
        }
    }

    let mut best_paths_queue = VecDeque::new();
    let mut best_path_squares = FxHashSet::default();
    let mut best_path_seen = FxHashSet::default();
    best_paths_queue.push_front((best_score_found, end, RCDirection::down()));
    best_paths_queue.push_front((best_score_found, end, RCDirection::left()));

    while let Some((score, pos, dir)) = best_paths_queue.pop_front() {
        let opposite_dir = dir.clockwise().clockwise();
        match seen.get(&(pos, opposite_dir)) {
            Some(&s) if s == score => {},
            _ => continue,
        }

        best_path_squares.insert(pos);

        if !best_path_seen.insert((pos, dir)) {
            continue;
        }


        let char_in_front = grid[pos + dir];
        if char_in_front == '.' || char_in_front == 'S' {
            best_paths_queue.push_front((score - 1, pos + dir, dir));
        }

        let dir_left = dir.counterclockwise();
        if grid[pos + dir_left] == '.' {
            best_paths_queue.push_front((score - 1000, pos, dir_left));
        }
        let dir_right = dir.clockwise();
        if grid[pos + dir_right] == '.' {
            best_paths_queue.push_front((score - 1000, pos, dir_right));
        }

    }


    let part1 = best_score_found;
    let part2 = best_path_squares.len() as u64;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 7036);
    assert_eq!(part2, 45);
    Ok(())
}
