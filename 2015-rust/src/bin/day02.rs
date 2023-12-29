use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    let input = get_input(2015, 2)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let sizes = input
        .trim()
        .lines()
        .map(|l| {
            let mut sizes = all_numbers(l);
            sizes.sort();
            assert_eq!(sizes.len(), 3);
            sizes.into_iter().next_tuple::<(_, _, _)>().unwrap()
        })
        .collect_vec();

    let part1 = sizes
        .iter()
        .map(|&ss| {
            let (s, m, l) = ss;
            3 * (s * m) + 2 * (m * l) + 2 * (s * l)
        })
        .sum::<u32>();
    let part2 = sizes
        .iter()
        .map(|&ss| {
            let (s, m, l) = ss;
            (2 * s) + (2 * m) + (s * m * l)
        })
        .sum::<u32>();

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
