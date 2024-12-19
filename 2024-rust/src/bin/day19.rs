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

fn solve_for(input: &str) -> Result<(usize, usize)> {
    let (towels, patterns) = input.trim().split_once("\n\n").unwrap();
    let towels = towels.split(", ").collect_vec();
    let patterns = patterns.lines().collect_vec();

    let mut cache = FxHashMap::default();
    let mut part1 = 0;
    let mut part2 = 0;
    for pattern in patterns {
        let possibilities = count_possibilities(pattern, &mut cache, &towels);
        if possibilities > 0 {
            part1 += 1;
        }
        part2 += possibilities
    }

    Ok((part1, part2))
}

fn count_possibilities<'i>(
    to_match: &'i str,
    cache: &mut FxHashMap<&'i str, usize>,
    towels: &[&'i str],
) -> usize {
    if let Some(&res) = cache.get(to_match) {
        return res;
    }

    if to_match.is_empty() {
        return 1;
    }

    let res = towels
        .iter()
        .map(|t| {
            if let Some(remaining) = to_match.strip_prefix(t) {
                count_possibilities(remaining, cache, towels)
            } else {
                0
            }
        })
        .sum();

    cache.insert(to_match, res);
    res
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
    assert_eq!(part2, 16);
    Ok(())
}
