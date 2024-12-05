use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 5)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let (rules, updates) = input.trim().split_once("\n\n").unwrap();

    let prerequisites_of: HashMap<u32, Vec<u32>> = rules
        .lines()
        .map(all_numbers)
        .map(|ns| (ns[0], ns[1]))
        .fold(HashMap::default(), |mut acc, (before, after)| {
            acc.entry(after).or_default().push(before);
            acc
        });

    let updates = updates.lines().map(all_numbers).collect_vec();

    let empty = Vec::<u32>::new();
    let mut ordered_updates = 0;
    let mut unordered_updates = 0;
    for mut update in updates {
        let mut seen = HashSet::new();
        let mut is_ordered_already = true;
        for entry in &update {
            if seen
                .iter()
                .any(|s| prerequisites_of.get(s).unwrap_or(&empty).contains(entry))
            {
                // we broke a rule (one of the previously-seen items has this entry as a prerequisite, so they're the wrong way round)
                is_ordered_already = false;
                break;
            }

            seen.insert(*entry);
        }
        // all entries validated

        if is_ordered_already {
            // len/2 produces the correct index even though integer division
            // gets truncated downwards, because the len is 1-based but the index
            // we want is 0-based
            assert!(&update.len() % 2 == 1);
            ordered_updates += update[update.len() / 2];
        } else {
            update.sort_by(|a, b| {
                if prerequisites_of.get(a).unwrap_or(&empty).contains(b) {
                    Ordering::Less
                } else if prerequisites_of.get(b).unwrap_or(&empty).contains(a) {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            });
            unordered_updates += update[update.len() / 2];
        }
    }

    // println!("digraph {{");
    // for (after, before) in prerequisites_of.iter() {
    //     for b in before.iter() {
    //         println!("{} -> {}", after, b);
    //     }
    // }
    // println!("}}");

    Ok((ordered_updates.into(), unordered_updates.into()))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 143);
    assert_eq!(part2, 123);
    Ok(())
}
