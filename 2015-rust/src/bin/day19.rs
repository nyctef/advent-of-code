use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::HashSet;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 19)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let (replacements, target) = input.trim().split_once("\n\n").unwrap();
    let replacements = replacements
        .lines()
        .map(|l| l.split_once(" => ").unwrap())
        .collect_vec();

    let mut calibration_results = HashSet::new();

    for &(source, dest) in &replacements {
        for (i, _) in target.match_indices(source) {
            let mut new_string = target.to_string();
            new_string.replace_range(i..(i + source.len()), dest);
            calibration_results.insert(new_string);
        }
    }

    let part1 = calibration_results.len();

    let mut search = Dijkstra::new(|s: &State| s.molecule.clone());
    search.push(State::new(target.to_string(), 0));
    let res = search.run_single(
        |s| {
            let mut candidates = vec![];

            for &(source, dest) in &replacements {
                for (i, _) in s.molecule.match_indices(dest) {
                    let mut new_string = s.molecule.to_string();
                    new_string.replace_range(i..(i + dest.len()), source);
                    candidates.push(State::new(new_string, s.steps + 1));
                }
            }

            candidates
        },
        |s| s.molecule == "e",
    );

    let part2 = res.steps;
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Constructor)]
struct State {
    molecule: String,
    steps: usize,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.steps.cmp(&other.steps)
    }
}
impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: ", result);
    Ok(())
}
