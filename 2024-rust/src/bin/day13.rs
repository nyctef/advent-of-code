use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 13)?;

    let part1 = solve_for(&input, false)?;
    let part2 = solve_for(&input, true)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str, part2: bool) -> Result<isize> {
    let machines = input
        .trim()
        .split("\n\n")
        .map(|m| {
            let lines = m.lines().map(all_numbers_isize).collect_vec();
            // TODO: should probably have more generic position/direction structs
            // rather than stealing these from CharGrid
            let a = RCDirection::new(lines[0][0], lines[0][1]);
            let b = RCDirection::new(lines[1][0], lines[1][1]);
            let mut target = CharGridIndexRC::new(
                lines[2][0].try_into().unwrap(),
                lines[2][1].try_into().unwrap(),
            );

            if part2 {
                target = target + RCDirection::new(10000000000000, 10000000000000);
            }

            (a, b, target)
        })
        .collect_vec();

    let mut total_cost = 0;

    for (a_move, b_move, target) in machines {
        let b = ((target.col as isize * a_move.rowdiff) - (a_move.coldiff * target.row as isize))
            / ((b_move.coldiff * a_move.rowdiff) - (b_move.rowdiff * a_move.coldiff));
        let a = (target.col as isize - (b * b_move.coldiff)) / (a_move.coldiff);

        // we found a solution to the equation, but we're not guaranteed that it's a valid
        // solution when we're limited to integer values (no half A presses allowed) so
        // check whether we actually land on the target or not:
        let hits = CharGridIndexRC::zero() + (a_move * a) + (b_move * b) == target;

        if hits {
            total_cost += (3 * a) + b;
        }
    }

    Ok(total_cost)
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
    let result = solve_for(input, false)?;

    assert_eq!(result, 480);
    Ok(())
}
