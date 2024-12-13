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
            let target = CharGridIndexRC::new(
                lines[2][0].try_into().unwrap(),
                lines[2][1].try_into().unwrap(),
            );

            (a, b, target)
        })
        .collect_vec();

    let mut part1 = 0;

    for (a_move, b_move, target) in machines {
        let mut cheapest_win_found = u64::MAX;
        let mut search = VecDeque::new();
        let mut seen = FxHashSet::default();

        dbg!(a_move, b_move, target);

        let mut progress = 0;
        search.push_back((0, 0));
        while let Some((a, b)) = search.pop_front() {
            if !seen.insert((a, b)) {
                continue;
            }
            if progress % 10_000 == 0 {
                eprintln!(
                    "len {} cheapest {} | a {} b {}",
                    search.len(),
                    cheapest_win_found,
                    a,
                    b
                );
            }
            progress += 1;
            let cost = 3 * a + 1 * b;
            if cost >= cheapest_win_found {
                continue;
            }
            let position = CharGridIndexRC::zero() + (a_move * a as isize) + (b_move * b as isize);
            if position.row > target.row || position.col > target.col {
                continue;
            }

            if position.row == target.row && position.col == target.col {
                cheapest_win_found = cost;
                continue;
            }

            search.push_back((a + 1, b));
            search.push_back((a, b + 1));
        }

        eprintln!("{}", cheapest_win_found);
        if cheapest_win_found != u64::MAX {
            part1 += cheapest_win_found;
        }
    }

    let part2 = 0;
    Ok((part1, part2))
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
