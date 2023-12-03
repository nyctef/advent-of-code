use std::collections::HashMap;

use crate::aoc_util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use regex::Regex;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 3)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let digits_re = Regex::new(r"\d+").unwrap();
    let lines = input.trim().lines().collect_vec();
    let rows = lines.len();
    let cols = lines[0].len();

    let mut symbol_map: HashMap<(usize, usize), (char, Vec<u32>)> = HashMap::new();

    dbg!(rows, cols);
    let mut total = 0;

    for (line_no, line) in lines.iter().enumerate() {
        for num_match in digits_re.find_iter(line) {
            let num_value: u32 = str::parse(num_match.as_str()).unwrap();
            let left = num_match.start().saturating_sub(1);
            let top = line_no.saturating_sub(1);
            let right = num_match.end().min(cols - 1);
            let bottom = (line_no + 1).min(rows - 1);
            // dbg!(num_match, left, top, right, bottom);

            let mut symbol_found = false;
            'search: for row_to_check in top..=bottom {
                for col_to_check in left..=right {
                    let char = lines[row_to_check].chars().nth(col_to_check).unwrap();
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
                // eprintln!("num seems to be missing a symbol {:?}", num_match);
            }
        }
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
