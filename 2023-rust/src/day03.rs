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
    dbg!(rows, cols);
    let mut total = 0;

    for (line_no, line) in lines.iter().enumerate() {
        for num_match in digits_re.find_iter(line) {
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
                        break 'search;
                    }
                }
            }

            if (symbol_found) {
                let num_value: u32 = str::parse(num_match.as_str()).unwrap();
                total += num_value;
                // eprintln!("found num {num_value}")
            } else {
                // eprintln!("num seems to be missing a symbol {:?}", num_match);
            }
        }
    }

    Ok(total.to_string())
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

    assert_eq!("4361", result);
    Ok(())
}
