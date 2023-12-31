use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::collections::HashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 13)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for<'i>(input: &'i str) -> Result<String> {
    let mut names: Vec<&'i str> = input
        .trim()
        .lines()
        .map(|l| l.split_once(' ').unwrap().0)
        .unique()
        .collect_vec();
    let mut happiness_changes: HashMap<&'i str, Vec<(&'i str, isize)>> = input
        .trim()
        .lines()
        .map(|l| {
            let l = l.trim_end_matches('.');
            let (p1, rest) = l.split_once(' ').unwrap();
            let lose = rest.contains("lose");
            let mut value = *all_numbers_isize(rest).iter().exactly_one().unwrap();
            if lose {
                value = -value;
            }
            let (_rest, p2) = rest.split_once("next to ").unwrap();

            (p1, (p2, value))
        })
        .into_group_map();

    let part1 = find_best_arrangement(&names, &happiness_changes);

    for name in &names {
        happiness_changes
            .get_mut(name)
            .unwrap()
            .push(("yourself", 0));
    }
    happiness_changes.insert(
        "yourself",
        names.iter().copied().map(|n| (n, 0)).collect_vec(),
    );
    names.push("yourself");

    let part2 = find_best_arrangement(&names, &happiness_changes);
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn find_best_arrangement(
    names: &Vec<&str>,
    happiness_changes: &HashMap<&str, Vec<(&str, isize)>>,
) -> isize {
    let mut best = 0;
    for candidate in names.iter().copied().permutations(names.len()) {
        let mut score = 0;
        for c in 0..candidate.len() as isize {
            let p = candidate[c as usize];
            let n1 = candidate[((c - 1).rem_euclid(names.len() as isize)) as usize];
            let n2 = candidate[((c + 1).rem_euclid(names.len() as isize)) as usize];

            let changes = &happiness_changes[p];
            score += changes.iter().find(|x| x.0 == n1).unwrap().1;
            score += changes.iter().find(|x| x.0 == n2).unwrap().1;
        }
        if score > best {
            best = score;
        }
    }
    best
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 330 | Part 2: 286", result);
    Ok(())
}
