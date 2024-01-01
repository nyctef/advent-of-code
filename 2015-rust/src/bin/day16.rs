use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::collections::HashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 16)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let aunts = input
        .trim()
        .lines()
        .map(|l| {
            let l = l.strip_prefix("Sue ").unwrap();
            let (n, l) = l.split_once(": ").unwrap();
            let properties = l
                .split(", ")
                .map(|p| p.split_once(": ").unwrap())
                .map(|(k, v)| (k, v.parse::<usize>().unwrap()))
                .collect::<HashMap<_, _>>();
            (n.parse::<usize>().unwrap(), properties)
        })
        .collect_vec();

    let to_match = [
        ("children", 3),
        ("cats", 7),
        ("samoyeds", 2),
        ("pomeranians", 3),
        ("akitas", 0),
        ("vizslas", 0),
        ("goldfish", 5),
        ("trees", 3),
        ("cars", 2),
        ("perfumes", 1),
    ];

    let mut part1 = 0;
    'aunt: for a in &aunts {
        for m in to_match {
            if let Some(v) = a.1.get(m.0) {
                if *v != m.1 {
                    continue 'aunt;
                }
            }
        }
        // no mismatches were found, and we assume there's a unique solution
        part1 = a.0;
        break;
    }

    let mut part2 = 0;
    'aunt2: for a in &aunts {
        // println!("testing aunt {:?}", a);
        for m in to_match {
            if let Some(v) = a.1.get(m.0) {
                if m.0 == "cats" || m.0 == "trees" {
                    if *v <= m.1 {
                        continue 'aunt2;
                    }
                } else if m.0 == "pomeranians" || m.0 == "goldfish" {
                    if *v >= m.1 {
                        continue 'aunt2;
                    }
                } else if *v != m.1 {
                    continue 'aunt2;
                }
            }
        }
        part2 = a.0;
        break;
    }
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}
