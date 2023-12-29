use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;

pub fn main() -> Result<()> {
    let input = get_input(2015, 2)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let part1 = input.trim().lines().map(|l| {
        let mut sizes = all_numbers(l);
        sizes.sort();
        assert_eq!(sizes.len(), 3);
        3 * (sizes[0] * sizes[1]) + 2 * (sizes[1] * sizes[2]) + 2 * (sizes[0] * sizes[2])
    }).sum::<u32>();

    let part2 = input.trim().lines().map(|l| {
        let mut sizes = all_numbers(l);
        sizes.sort();
        assert_eq!(sizes.len(), 3);
        2 * sizes[0] + 2 * sizes[1] + sizes[0] * sizes[1] * sizes[2]
    }).sum::<u32>();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
2x3x4
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 58 | Part 2: 34", result);
    Ok(())
}
