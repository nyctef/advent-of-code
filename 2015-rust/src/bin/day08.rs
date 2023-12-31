use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 8)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut string_literal_total = 0;
    let mut string_value_total = 0;

    for line in input.trim().lines() {
        string_literal_total += line.len();
        let mut chars = line.chars();
        // assume line starts and ends with `"`
        chars.next();
        chars.next_back();
        while let Some(c) = chars.next() {
            if c == '\\' {
                let c2 = chars.next().unwrap();
                if c2 == 'x' {
                    // hex escape
                    let (_, _) = chars.next_tuple().unwrap();
                }
                string_value_total += 1;
            } else {
                // regular character
                string_value_total += 1;
            }
        }
    }

    let part1 = string_literal_total - string_value_total;

    let mut escapes_needed = 0;
    let mut extra_quote_chars_needed = 0;
    for line in input.trim().lines() {
        escapes_needed += line.chars().filter(|&c| c == '"' || c == '\\').count();

        extra_quote_chars_needed += 2;
    }
    let part2 = escapes_needed + extra_quote_chars_needed;
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
""
"abc"
"aaa\"aaa"
"\x27"
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 12 | Part 2: 19", result);
    Ok(())
}
