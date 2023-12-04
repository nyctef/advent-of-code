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

    let mut symbol_map: HashMap<CharGridIndexRC, (char, Vec<u32>)> = HashMap::new();

    let mut total = 0;
    // let mut spans_missing_symbols = Vec::new();

    for (span, num_value) in grid.enumerate_numbers() {
        let search_span = span.grow_1();
        // dbg!((span, num_value, left, top, right, bottom));

        let mut symbol = None;
        'search: for (search_pos, char) in grid.enumerate_range_rc(search_span) {
            if char != '.' && !char.is_ascii_digit() {
                symbol = Some(char);
                let map_entry = symbol_map.entry(search_pos).or_default();
                map_entry.0 = char;
                map_entry.1.push(num_value);
                break 'search;
            }
        }

        if let Some(_sym) = symbol {
            total += num_value;
            // println!("found num {num_value} with symbol {sym}")
        } else {
            // spans_missing_symbols.push(span);
            // println!("found num {num_value} without a symbol");
        }
    }

    // for m in spans_missing_symbols {
    //     grid.set_range_rc(m, '█');
    // }
    // dbg!(grid);

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

#[test]
fn test_num_at_end_of_line() -> Result<()> {
    let input = r###"
...*123
.......
"###;
    let result = solve_for(input)?;

    assert_eq!(format!("Part 1: {} | Part 2: 0", 123), result);
    Ok(())
}

#[test]
fn test_distant_symbol() -> Result<()> {
    let input = r###"
.......
.826...
.....*.
"###;
    let result = solve_for(input)?;

    assert_eq!(format!("Part 1: 0 | Part 2: 0"), result);
    Ok(())
}
