use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::iter;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 21)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, u64)> {
    let codes = input.trim().lines().collect_vec();

    let keypad = CharGrid::from_string(
        r###"
789
456
123
.0A
"###,
    );
    let keypad_start = keypad.find_single_char('A');
    let directions = CharGrid::from_string(
        r###"
.^A
<v>
"###,
    );
    let directions_start = directions.find_single_char('A');

    let mut part1 = 0;
    for code in codes {
        eprintln!("code: {:?}", code);
        let first_robot_moves = track_moves(keypad_start, code.chars().collect_vec(), &keypad);
        eprintln!("first robot: {:?}", first_robot_moves);

        let second_robot_moves = track_moves(directions_start, first_robot_moves, &directions);
        eprintln!("second robot: {:?}", second_robot_moves);

        let third_robot_moves = track_moves(directions_start, second_robot_moves, &directions);
        eprintln!("third robot: {:?}", third_robot_moves);

        let complexity = third_robot_moves.len() * code.trim_matches('A').parse::<usize>().unwrap();
        eprintln!("complexity: {}", complexity);
        part1 += complexity;
    }

    let part2 = 0;
    Ok((part1, part2))
}

fn track_moves(start: CharGridIndexRC, target: Vec<char>, map: &CharGrid) -> Vec<char> {
    let mut robot_pos = start;
    let mut result = vec![];
    for &t in &target {
        let target_pos = map.find_single_char(t);
        let dist = RCDirection::from_to(robot_pos, target_pos);
        if dist.coldiff < 0 {
            result.extend(iter::repeat_n('<', dist.coldiff.abs() as usize));
        } else if dist.coldiff > 0 {
            result.extend(iter::repeat_n('>', dist.coldiff.abs() as usize));
        }
        if dist.rowdiff < 0 {
            result.extend(iter::repeat_n('^', dist.rowdiff.abs() as usize));
        } else if dist.rowdiff > 0 {
            result.extend(iter::repeat_n('v', dist.rowdiff.abs() as usize));
        }

        result.push('A');
        robot_pos = target_pos;
    }
    result
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
029A
980A
179A
456A
379A
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 126384);
    assert_eq!(part2, 0);
    Ok(())
}
