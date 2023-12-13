use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 13)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let patterns = input.split("\n\n").map(|x| x.trim());

    let mut part1 = 0;
    let mut part2 = 0;
    for pattern in patterns {
        part1 += solve_for_pattern(pattern, 0);
        part2 += solve_for_pattern(pattern, 1);
    }

    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn solve_for_pattern(input: &str, target_score: u32) -> u32 {
    let grid = CharGrid::from_string(input);
    for r in 0..grid.height() - 1 {
        if score_row_mirror(&grid, r) == target_score {
            return (r as u32 + 1) * 100;
        }
    }
    for c in 0..grid.width() - 1 {
        if score_col_mirror(&grid, c) == target_score {
            return c as u32 + 1;
        }
    }
    panic!("no mirrors found");
}

fn score_row_mirror(grid: &CharGrid, start: usize) -> u32 {
    // l -> above, r -> below
    let mut l = start;
    let mut r = start + 1;
    let all_lines = grid.lines().collect_vec();
    let mut score = 0;
    while r < grid.height() {
        for (lc, rc) in all_lines[l].chars().zip(all_lines[r].chars()) {
            if lc != rc {
                // an imperfection! let's see if we only get one...
                score += 1
            }
        }

        // since l is usize it can't actually go below 0, so we have to check manually here
        if l == 0 {
            break;
        }
        l -= 1;
        r += 1;
    }
    score
}

fn score_col_mirror(grid: &CharGrid, start: usize) -> u32 {
    let mut l = start;
    let mut r = start + 1;
    let all_cols = grid.cols().collect_vec();
    let mut score = 0;
    while r < grid.width() {
        for (lc, rc) in all_cols[l].iter().zip(all_cols[r].iter()) {
            if lc != rc {
                // an imperfection! let's see if we only get one...
                score += 1
            }
        }

        // since l is usize it can't actually go below 0, so we have to check manually here
        if l == 0 {
            break;
        }
        l -= 1;
        r += 1;
    }
    score
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.
"###;
    let result = solve_for_pattern(input, 0);

    assert_eq!(5, result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"###;
    let result = solve_for_pattern(input, 0);

    assert_eq!(400, result);
    Ok(())
}
