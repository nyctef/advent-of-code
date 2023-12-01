use crate::aoc_util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 1)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9"];
    let digit_strings = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    let digit_re = regex::Regex::new(&format!(
        r"({})|({})",
        digits.join("|"),
        digit_strings.join("|"),
    ))?;

    let mut sum = 0;
    for line in input.lines() {
        if line.is_empty() {
            continue;
        }

        let mut first_digit = digit_re.find(line).unwrap().as_str();
        if let Some(i) = digit_strings.iter().position(|&s| s == first_digit) {
            first_digit = digits[i];
        }

        let mut last_digit = digit_re.find_iter(line).last().unwrap().as_str();
        if let Some(i) = digit_strings.iter().position(|&s| s == last_digit) {
            last_digit = digits[i];
        }

        let number = format!("{}{}", first_digit, last_digit);
        sum += number.parse::<u32>()?;
    }
    Ok(sum.to_string())
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"###;
    let result = solve_for(input)?;

    assert_eq!("142", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"###;
    let result = solve_for(input)?;

    assert_eq!("281", result);
    Ok(())
}
