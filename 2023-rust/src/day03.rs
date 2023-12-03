use crate::utils::*;
use color_eyre::eyre::Result;
use std::collections::HashMap;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 3)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);

    let mut symbol_map: HashMap<(usize, usize), (char, Vec<u32>)> = HashMap::new();

    // dbg!(&grid, &digits_re);
    let mut total = 0;

    let mut limit: i32 = 30;

    for (span, num_value) in grid.enumerate_numbers() {
        let left = span.start.col.saturating_sub(1);
        let top = span.start.row.saturating_sub(1);
        let right = (span.end + 1).col.min(grid.width() - 1);
        let bottom = (span.end + 1).row.min(grid.height() - 1);
        //dbg!((span, num_value, left, top, right, bottom));

        let mut symbol_found = false;
        'search: for row_to_check in top..=bottom {
            for col_to_check in left..=right {
                let char = grid.index_rc(row_to_check, col_to_check);
                if char != '.' && !char.is_digit(10) {
                    symbol_found = true;
                    let map_entry = symbol_map.entry((row_to_check, col_to_check)).or_default();
                    map_entry.0 = char;
                    map_entry.1.push(num_value);
                    break 'search;
                }
            }
        }

        if symbol_found {
            total += num_value;
            // eprintln!("found num {num_value}")
        } else {
            // eprintln!("{num_value} seems to be missing a symbol {:?}", span);
        }

        // limit -= 1;
        // if limit <= 0 {
        //     break;
        // }
    }

    let mut gear_ratios = 0;
    // dbg!(symbol_map.values());
    for (_key, map_entry) in symbol_map.iter() {
        if map_entry.0 == '*' && map_entry.1.len() == 2 {
            gear_ratios += map_entry.1[0] * map_entry.1[1];
        }
    }
    Ok(format!("Part 1: {total} | Part 2: {gear_ratios}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 4361 | Part 2: 467835", result);
    Ok(())
}

#[test]
fn test_breaking_numbers_across_rows() -> Result<()> {
    let input = r###"
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
........12
34-.......
"###;
    let result = solve_for(input)?;

    assert_eq!(format!("Part 1: {} | Part 2: 467835", 4361 + 34), result);
    Ok(())
}
