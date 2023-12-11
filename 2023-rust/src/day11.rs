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
            expanded_rows_grid.append_str_row(&line);
        }
    }
    dbg!(&expanded_rows_grid);
    for col in expanded_rows_grid.cols() {
        dbg!(&col);
        expanded_cols_grid.append_chars_col(&col);
        if col.iter().all(|c| *c == '.') {
            expanded_cols_grid.append_chars_col(&col);
        }
    }

    dbg!(&expanded_cols_grid);

    todo!();
    let part1 = "";
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
