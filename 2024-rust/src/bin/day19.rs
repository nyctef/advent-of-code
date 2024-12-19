use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 19)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, u64)> {
    let (towels, patterns) = input.trim().split_once("\n\n").unwrap();
    let towels = towels.split(", ").collect_vec();
    let patterns = patterns.lines().collect_vec();

    let mut cache = FxHashMap::default();
    let part1 = patterns.iter().filter(|p| is_possible(p, &mut cache, &towels)).count();

    let part2 = 0;
    Ok((part1, part2))
}

fn is_possible(p: &str, cache: &mut FxHashMap<&str, bool>, towels: &[&str]) -> bool {
    if let Some(&res) = cache.get(p) {
        return res;
    }

    if p.len() == 0 {
        return true;
    }

    return towels.iter().any(|t| p.starts_with(t) && is_possible(&p[t.len()..], cache, towels));

}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 6);
    assert_eq!(part2, 0);
    Ok(())
}
