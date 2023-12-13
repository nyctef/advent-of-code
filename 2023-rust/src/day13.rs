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
        // println!("testing row {r}");
        if is_row_mirror(&grid, r) {
            return (r as u32 + 1) * 100;
        }
    }
    for c in 0..grid.width() - 1 {
        // println!("testing col {c}");
        if is_col_mirror(&grid, c) {
            return (c as u32 + 1);
        }
    }
    panic!("no mirrors found");
}

fn is_row_mirror(grid: &CharGrid, start: usize) -> bool {
    // l -> above, r -> below
    let mut l = start;
    let mut r = start + 1;
    let all_lines = grid.lines().collect_vec();
    while l >= 0 && r < grid.height() {
        //         println!(
        //           "comparing lines {}:{} and {}:{}",
        //         l, all_lines[l], r, all_lines[r]
        //   );
        if all_lines[l] != all_lines[r] {
            return false;
        }

        // l -= 1;
        if l == 0 {
            break;
        }
        l -= 1;
        r += 1;
    }
    return true;
}

fn is_col_mirror(grid: &CharGrid, start: usize) -> bool {
    let mut l = start;
    let mut r = start + 1;
    let all_cols = grid.cols().collect_vec();
    while l >= 0 && r < grid.width() {
        //println!(
        //    "comparing cols {}:{:?} and {}:{:?}",
        //    l, all_cols[l], r, all_cols[r]
        //);
        if all_cols[l] != all_cols[r] {
            return false;
        }

        // l -= 1;
        if l == 0 {
            break;
        }
        l -= 1;
        r += 1;
    }
    return true;
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
