use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 17)?;

    let result = solve_for(&input, 150)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, target: usize) -> Result<String> {
    let containers = input
        .trim()
        .lines()
        .map(|l| l.parse::<usize>().unwrap())
        .collect_vec();
    let mut count = 0;
    let mut part2 = 0;
    for k in 0..=containers.len() {
        for choice in containers.iter().combinations(k) {
            if choice.into_iter().sum::<usize>() == target {
                count += 1;
            }
        }
        if part2 == 0 && count != 0 {
            part2 = count;
        }
    }
    let part1 = count;
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
20
15
10
5
5
"###;
    let result = solve_for(input, 25)?;

    assert_eq!("Part 1: 4 | Part 2: 3", result);
    Ok(())
}
