use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;

pub fn main() -> Result<()> {
    let input = get_input(2015, 1)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {

    let mut floor_count = 0;
    let mut first_basement_position = 0;
    for (p, c) in input.trim().chars().enumerate() {
        floor_count += if c == '(' { 1 } else { -1 };
        if floor_count == -1 && first_basement_position == 0 {
            first_basement_position = p + 1;
        }
    }

    Ok(format!("Part 1: {floor_count} | Part 2: {first_basement_position}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
))(((((
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 3 | Part 2: 1", result);
    Ok(())
}
