use std::iter;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 21)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
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

    for code in codes {
        let mut first_robot_pos = keypad_start;
        let mut first_robot_moves = vec![];
        for code_char in code.chars() {
            let target = keypad.find_single_char(code_char);
            let dist = RCDirection::from_to(first_robot_pos, target);
            if dist.coldiff < 0 {
                first_robot_moves.extend(iter::repeat_n('<', dist.coldiff.abs() as usize));
            } else if dist.coldiff > 0 {
                first_robot_moves.extend(iter::repeat_n('>', dist.coldiff.abs() as usize));
            }
            if dist.rowdiff < 0 {
                first_robot_moves.extend(iter::repeat_n('^', dist.rowdiff.abs() as usize));
            } else if dist.rowdiff > 0 {
                first_robot_moves.extend(iter::repeat_n('v', dist.rowdiff.abs() as usize));
            }

            first_robot_moves.push('A');
            first_robot_pos = target;
        }
        dbg!(&first_robot_moves);
    }

    let part1 = 0;
    let part2 = 0;
    Ok((part1, part2))
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
