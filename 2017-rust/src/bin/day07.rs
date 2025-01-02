use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 7)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (&str, u64) {
    let mut part2 = 0;

    let mut parents = FxHashMap::default();
    let mut weights = FxHashMap::<&str, u32>::default();
    for line in input.trim().lines() {
        if let Some((left, right)) = line.split_once(" -> ") {
            let above = right.split(", ");
            let (below, weight) = left.split_once(" ").unwrap();
            let weight = all_numbers(weight)[0];
            weights.insert(below, weight);
            for a in above {
                parents.insert(a, below);
            }
        }
    }

    let mut part1 = weights.keys().next().unwrap();
    let mut part1_parent = parents.get(part1);
    while part1_parent.is_some() {
        part1 = part1_parent.unwrap();
        part1_parent = parents.get(part1);
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, "tknk");
    assert_eq!(part2, 0);
}
