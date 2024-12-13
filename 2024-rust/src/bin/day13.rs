use std::collections::VecDeque;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashSet;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 13)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let machines = input
        .trim()
        .split("\n\n")
        .map(|m| {
            let lines = m.lines().map(|l| all_numbers_isize(l)).collect_vec();
            // TODO: should probably have more generic position/direction structs
            // rather than stealing these from CharGrid
            let a = RCDirection::new(lines[0][0], lines[0][1]);
            let b = RCDirection::new(lines[1][0], lines[1][1]);
            let mut target = CharGridIndexRC::new(
                lines[2][0].try_into().unwrap(),
                lines[2][1].try_into().unwrap(),
            );

            target = target + RCDirection::new(10000000000000, 10000000000000);

            (a, b, target)
        })
        .collect_vec();

    let mut part1 = 0;

    let mut m = 0;

    for (a_move, b_move, target) in machines {
        let b = ((target.col as isize * a_move.rowdiff) - (a_move.coldiff * target.row as isize))
            / ((b_move.coldiff * a_move.rowdiff) - (b_move.rowdiff * a_move.coldiff));
        let a = (target.col as isize - (b * b_move.coldiff)) / (a_move.coldiff);

        m += 1;

        let hits = CharGridIndexRC::zero() + (a_move * a) + (b_move * b) == target;
        
        if (hits) {
        part1 += (3*a) + b;
        }
        dbg!(m, a, b, (3*a) + b, hits);
    }

    let part2 = 0;
    Ok((part1 as u64, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 480);
    assert_eq!(part2, 0);
    Ok(())
}
