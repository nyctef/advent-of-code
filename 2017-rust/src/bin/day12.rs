use std::collections::VecDeque;

use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 12)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (usize, u64) {
    let mut part1 = 0;
    let mut part2 = 0;

    let mut connections = FxHashMap::default();
    for line in input.trim().lines() {
        let (source, dests) = line.split_once(" <-> ").unwrap();
        let dests = dests.split(", ").collect_vec();
        connections.insert(source, dests);
    }

    let mut remaining = FxHashSet::from_iter(connections.keys());

    loop {
        if remaining.len() == 0 {
            break;
        }
        let next_start = remaining.iter().cloned().next().unwrap();
        let mut seen = FxHashSet::default();
        let mut search = VecDeque::default();
        search.push_front(next_start);
        while let Some(current) = search.pop_front() {
            if !seen.insert(current) {
                continue;
            }
            remaining.remove(&current);

            for connected in &connections[current] {
                search.push_back(connected);
            }
        }

        if seen.contains(&"0") {
            part1 = seen.len();
        }
        part2 += 1;
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 6);
    assert_eq!(part2, 2);
}
