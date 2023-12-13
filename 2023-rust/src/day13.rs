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

    let mut total = 0;
    for pattern in patterns {

        println!("----");
        println!("{}", pattern);
        println!("----");
        total += solve_for_pattern(pattern);
    }

    let part1 = total;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn solve_for_pattern(input: &str) -> u32 {
    let grid = CharGrid::from_string(input);
    for r in 0..grid.height() - 1 {
        let row_score = score_row_mirror(&grid, r);
        println!("testing row {r}: got score {row_score}");
        if row_score == 1 {
            return (r as u32 + 1) * 100;
        }
    }
    for c in 0..grid.width() - 1 {
        // println!("testing col {c}");
        if score_col_mirror(&grid, c) == 1 {
            return (c as u32 + 1);
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
    while l >= 0 && r < grid.height() {
        for (lc, rc) in all_lines[l].chars().zip(all_lines[r].chars()) {
            if lc != rc { score += 1 }
        }


        // l -= 1;
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
    while l >= 0 && r < grid.width() {
        for (lc, rc) in all_cols[l].iter().zip(all_cols[r].iter()) {
            if lc != rc { score += 1 }
        }

        // l -= 1;
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
    let result = solve_for_pattern(input);

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
    let result = solve_for_pattern(input);

    assert_eq!(400, result);
    Ok(())
}
