use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 16)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let mut grid = CharGrid::from_string(input);
    let mut best_score_found = u64::MAX;

    let mut best_score_at_point = FxHashMap::default();
    let mut queue = VecDeque::new();
    let start = grid.find_single_char('S');
    let end = grid.find_single_char('E');
    grid.set_index_rc(start, '.');
    grid.set_index_rc(end, '.');
    queue.push_back((0, start, RCDirection::right()));

    let mut overshot = 0;
    let mut cannot_catch_up = 0;
    let mut cannot_catch_up_with_one_turn = 0;
    let mut cannot_catch_up_with_two_turns = 0;
    let mut better_score_at_point = 0;
    let mut at_end = 0;

    while let Some((score, pos, dir)) = queue.pop_front() {
        if score > best_score_found {
            overshot += 1;
            continue;
        }
        let best_case_distance = RCDirection::from_to(pos, end).manhattan_abs() as u64;
        if score + best_case_distance > best_score_found {
            cannot_catch_up += 1;
            continue;
        }
        if (pos.row != end.row && pos.col != end.col)
            && (score + 1000 + best_case_distance) > best_score_found
        {
            cannot_catch_up_with_one_turn += 1;
            continue;
        }

        if (dir == RCDirection::down() || dir == RCDirection::left())
            && score + best_case_distance + 2000 > best_score_found
        {
            cannot_catch_up_with_two_turns += 1;
            continue;
        }

        let score_at_point = best_score_at_point.entry((pos, dir)).or_insert(score);
        if *score_at_point < score {
            better_score_at_point += 1;
            continue;
        }
        *score_at_point = score;
        if pos == end {
            at_end += 1;
            best_score_found = score;
            continue;
        }

        // this seems to be the best ordering: the turns being the latter call to `push_front`
        // means they're the ones that end up at the front of the queue, and the massive score
        // penalty for turning means those branches can be discarded more quickly
        if grid[pos + dir] == '.' {
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

    dbg!(
        overshot,
        cannot_catch_up,
        cannot_catch_up_with_one_turn,
        cannot_catch_up_with_two_turns,
        better_score_at_point,
        at_end
    );

    let mut best_paths_queue = VecDeque::new();
    let mut best_path_squares = FxHashSet::default();
    let mut best_path_seen = FxHashSet::default();
    best_paths_queue.push_front((best_score_found, end, RCDirection::down()));
    best_paths_queue.push_front((best_score_found, end, RCDirection::left()));

    while let Some((score, pos, dir)) = best_paths_queue.pop_front() {
        let opposite_dir = dir.clockwise().clockwise();
        match best_score_at_point.get(&(pos, opposite_dir)) {
            Some(&s) if s == score => {}
            _ => continue,
        }

        best_path_squares.insert(pos);

        if !best_path_seen.insert((pos, dir)) {
            continue;
        }

        let char_in_front = grid[pos + dir];
        if char_in_front == '.' {
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
