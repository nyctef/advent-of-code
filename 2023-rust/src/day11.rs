use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 11)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid1 = CharGrid::from_string(input);
    let mut expanded_rows_grid = CharGrid::empty();
    let mut expanded_cols_grid = CharGrid::empty();
    for line in grid1.lines() {
        expanded_rows_grid.append_str_row(&line);
        if line.chars().all(|c| c == '.') {
            for _ in 0..(1_000_000 - 1) {
                expanded_rows_grid.append_str_row(&line);
            }
        }
    }
    for col in expanded_rows_grid.cols() {
        expanded_cols_grid.append_chars_col(&col);
        if col.iter().all(|c| *c == '.') {
            for _ in 0..(1_000_000 - 1) {
                expanded_cols_grid.append_chars_col(&col);
            }
        }
    }

    let galaxies = expanded_cols_grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == '#')
        .collect_vec();
    let mut total_distance = 0;
    for g1 in &galaxies {
        for g2 in &galaxies {
            let distance = RCDirection::from_to(&g1.0, &g2.0);
            total_distance += distance.manhattan_abs();
        }
    }

    let part1 = total_distance / 2;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 374 | Part 2: ", result);
    Ok(())
}
