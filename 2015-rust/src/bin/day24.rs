use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashSet;
use std::{
    collections::VecDeque,
    hash::{DefaultHasher, Hash, Hasher},
    num::Wrapping,
};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 24)?;

    let part1 = solve_for(&input, 3)?;
    let part2 = solve_for(&input, 4)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
struct Group(FxHashSet<u64>);

impl Hash for Group {
    // based on https://stackoverflow.com/a/77085302
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut sum = Wrapping::default();
        for value in &self.0 {
            let mut hasher = DefaultHasher::new();
            Hash::hash(value, &mut hasher);
            sum += hasher.finish();
        }
        state.write_u64(sum.0);
    }
}

fn solve_for(input: &str, group_count: u64) -> Result<u64> {
    let package_weights = input
        .trim()
        .lines()
        .map(|l| *all_numbers_u64(l).iter().exactly_one().unwrap())
        .collect_vec();

    let total_weight = package_weights.iter().sum::<u64>();
    let weight_per_group = total_weight / group_count;
    assert!(weight_per_group * group_count == total_weight);

    let mut groups = FxHashSet::default();
    let mut group_search = VecDeque::new();
    group_search.push_front(Group::default());

    let mut smallest_group = usize::MAX;

    let mut counter = 0;

    while let Some(next) = group_search.pop_front() {
        counter += 1;
        if counter % 1_000_000 == 0 {
            eprintln!("c {} len {} wpg {} smallest {} | {:?}", counter, group_search.len(), weight_per_group, smallest_group, &next);

        }
        let weight = next.0.iter().sum::<u64>();
        if weight == weight_per_group {
            smallest_group = smallest_group.min(next.0.len());
            groups.insert(next);
            continue;
        }
        if weight > weight_per_group {
            continue;
        }
        if next.0.len() >= smallest_group {
            continue;
        }

        for &next_package in &package_weights {
            if next.0.contains(&next_package) {
                continue;
            }

            let mut candidate = next.clone();
            candidate.0.insert(next_package);
            group_search.push_front(candidate);
        }
    }

    // dbg!(&groups);

    let min_group_len = groups.iter().map(|g| g.0.len()).min().unwrap();
    let min_num_packages = groups
        .iter()
        .filter(|g| g.0.len() == min_group_len)
        .collect_vec();

    // dbg!(&min_num_packages);

    // for group1 in min_num_packages {
    //     let remaining_groups = groups
    //         .iter()
    //         .filter(|g| g.0.is_disjoint(&group1.0))
    //         .collect_vec();
    // }
    //
    let lowest_quantum = min_num_packages.iter().map(|g| g.0.iter().product()).min().unwrap();

    Ok(lowest_quantum)
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
1
2
3
4
5
7
8
9
10
11
"###;
    assert_eq!(solve_for(input, 3)?, 99);
    assert_eq!(solve_for(input, 4)?, 44);
    Ok(())
}
