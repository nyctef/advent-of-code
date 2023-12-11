use crate::utils::*;
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 11)?;

    let result = solve_for(&input, 1_000_000)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, expansion_factor: usize) -> Result<String> {
    let grid1 = CharGrid::from_string(input);
    let mut expanded_lines = vec![];
    let mut expanded_cols = vec![];
    for (l, line) in grid1.lines().enumerate() {
        if line.chars().all(|c| c == '.') {
            expanded_lines.push(l);
        }
    }
    for (c, col) in grid1.cols().enumerate() {
        if col.iter().all(|c| *c == '.') {
            expanded_cols.push(c);
        }
    }

    let mut galaxies = vec![];

    let expansion_factor = expansion_factor - 1;
    for (p, c) in grid1.enumerate_chars_rc() {
        if c != '#' {
            continue;
        }
        let num_expanded_lines = expanded_lines.iter().filter(|el| **el < p.row).count();
        let num_expanded_cols = expanded_cols.iter().filter(|el| **el < p.col).count();

        galaxies.push(CharGridIndexRC::new(
            p.row + num_expanded_lines * expansion_factor,
            p.col + num_expanded_cols * expansion_factor,
        ));
    }

    let mut total_distance = 0;
    for g1 in &galaxies {
        for g2 in &galaxies {
            let distance = RCDirection::from_to(&g1, &g2);
            total_distance += distance.manhattan_abs();
        }
    }

    let result = total_distance / 2;
    Ok(format!("Part 2: {result}"))
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
    let result = solve_for(input, 10)?;

    assert_eq!("Part 2: 1030", result);
    Ok(())
}
