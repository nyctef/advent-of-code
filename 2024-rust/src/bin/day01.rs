use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 1)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let nums = input.trim().lines().map(all_numbers).collect_vec();
    let mut list_1 = nums.iter().map(|n| n[0]).collect_vec();
    list_1.sort();
    let mut list_2 = nums.iter().map(|n| n[1]).collect_vec();
    list_2.sort();

    let part1 = list_1
        .iter()
        .zip_eq(list_2.iter())
        .map(|(&i1, &i2)| (i2 as i32 - i1 as i32).abs())
        .sum::<i32>();

    let list_2_occurrences = list_2
        .iter()
        .fold(FxHashMap::<u32, u32>::default(), |mut b, &n| {
            *(b.entry(n).or_default()) += 1;
            b
        });

    let list_1_similarities = list_1
        .iter()
        .map(|n| n * list_2_occurrences.get(n).unwrap_or(&0))
        .sum::<u32>();

    let part2 = list_1_similarities;
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
3   4
4   3
2   5
1   3
3   9
3   3
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 11 | Part 2: 31", result);
    Ok(())
}
