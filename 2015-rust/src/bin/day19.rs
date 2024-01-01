use aoc_2015_rust::util::*;
use color_eyre::{eyre::Result, owo_colors::OwoColorize};
use derive_more::Constructor;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::{collections::HashSet, hash::Hasher, cmp::Reverse};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 19)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let (replacements, target) = input.trim().split_once("\n\n").unwrap();
    let replacements = parse_replacements(replacements);
    let target = tokenize(target);

    // println!("r {:?} t {:?}", replacements, target);

    solve(&replacements, target);

    let part1 = "";
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn parse_replacements(replacements: &str) -> Vec<(&str, Vec<&str>)> {
    replacements
        .trim()
        .lines()
        .map(|l| l.split_once(" => ").unwrap())
        .map(|(s, d)| (s, tokenize(d)))
        .collect_vec()
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

fn tokenize(input: &str) -> Vec<&str> {
    let mut result = vec![];

    let mut i = 0;
    while i < input.len() {
        if i + 1 < input.len() && input.chars().nth(i + 1).unwrap().is_ascii_lowercase() {
            result.push(&input[i..i + 2]);
            i += 1;
        } else {
            result.push(&input[i..i + 1])
        }
        i += 1;
    }

    result
}

#[test]
fn test_tokenize() {
    assert_eq!(tokenize("ABC"), vec!["A", "B", "C"]);
    assert_eq!(tokenize("AnBrC"), vec!["An", "Br", "C"]);
}

/// given a string beginning with `token`, figure out which replacement rules we can use to generate
/// expansions beginning with `target_token`
/// returns a list of expansions along with the number of steps required to get each one
fn get_possible_expansions<'i>(
    replacements: &Vec<(&'i str, Vec<&'i str>)>,
    token: &'i str,
    target_token: &'i str,
) -> Vec<(usize, Vec<&'i str>)> {
    let mut result = vec![];

    let mut search: Search<(Vec<usize>, Vec<&str>), ()> = Search::new_exhaustive();
    // search state: (rules_applied, resulting_string)
    search.push((vec![], vec![token]));

    while let Some((rules_applied, resulting_string)) = search.pop() {
        if resulting_string.starts_with(&[target_token]) {
            result.push((rules_applied.len(), resulting_string.clone()));
        }

        for (i, possible_rule) in replacements.iter().enumerate() {
            if !rules_applied.contains(&i) && possible_rule.0 == resulting_string[0] {
                // todo bitvec would probably be better here
                let mut new_rule_list = rules_applied.clone();
                new_rule_list.push(i);
                search.push((
                    new_rule_list,
                    apply_rule(possible_rule, &resulting_string, 0),
                ));
            }
        }
    }

    result
}

#[test]
fn test_get_possible_expansions_1() {
    let replacements = parse_replacements(
        "
e => H
e => O
H => HO
H => OH
O => HH
",
    );

    assert_eq!(
        get_possible_expansions(&replacements, "e", "O"),
        vec![
            (1, vec!["O"]),
            (2, vec!["O", "H"]),
            (3, vec!["O", "H", "O"]),
            (3, vec!["O", "H", "H"]),
            (4, vec!["O", "H", "O", "H"])
        ]
    );
    assert_eq!(
        get_possible_expansions(&replacements, "e", "H"),
        vec![
            (1, vec!["H"]),
            (2, vec!["H", "O"]),
            (2, vec!["H", "H"]),
            (3, vec!["H", "H", "H"]),
            (3, vec!["H", "O", "H"]),
            (4, vec!["H", "H", "H", "O"]),
            (4, vec!["H", "O", "H", "H"])
        ]
    );
}

fn apply_rule<'i>(
    rule: &(&'i str, Vec<&'i str>),
    target: &Vec<&'i str>,
    position: usize,
) -> Vec<&'i str> {
    let (source, dest) = rule;
    let mut result = target.clone();
    assert!(result[position] == *source);
    result.splice(position..(position + 1), dest.iter().copied());
    result
}

fn solve<'i>(replacements: &Vec<(&'i str, Vec<&'i str>)>, target: Vec<&'i str>) {
    // let mut expansion_memo = FxHashMap::default();
    let mut search = Dijkstra::new(|s: &State2| (s.generated.clone(), s.solved));
    // state: generated string, solved prefix, num steps applied
    search.push(State2::new(vec!["e"], 0, 0));

    let mut counter = 0;

    let res = search.run_single(
        |s: State2| {
            let generated  = s.generated;
            let solved = s.solved;
            let steps = s.steps;
            let mut candidates = vec![];

            if solved >= target.len() && generated == target {
                println!(
                    "found potential solution {:?}",
                    (&generated, &solved, &steps)
                );
            }
            // if counter % 100 == 0 {
            //     // println!("search len: {}", search.len());
            //     println!("trying state {:?}", (&generated, solved, steps));
            // }
            // counter += 1;
            let target_token = target[solved];
            let expansion_index = solved;
            let expansions =
                // expansion_memo
                // .entry((generated[expansion_index], target_token))
                // .or_insert_with(|| {
                    get_possible_expansions(replacements, generated[expansion_index], target_token);
                // });

            for (cost, expansion) in expansions {
                let mut new_string = generated.clone();
                new_string.splice(expansion_index..(expansion_index + 1), expansion.clone());
                if new_string.len() > target.len() {
                    continue;
                }
                candidates.push(State2::new(new_string, solved + 1, steps + cost));
            }

            // println!("candidates: {:?}", candidates);
            candidates
        },
        |s| s.generated == target,
    );
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Constructor)]
struct State2<'i> {
    generated: Vec<&'i str>,
    solved: usize,
    steps: usize
}

impl<'i> Ord for State2<'i> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // (self.steps, Reverse(self.solved)).cmp(&(other.steps, Reverse(other.solved)))
         (Reverse(self.solved), self.steps).cmp(&(Reverse(other.solved), other.steps))
    }
}

impl<'i> PartialOrd for State2<'i> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[test]
fn test_example1() {
    let input = r"
e => H
e => O
H => HO
H => OH
O => HH

HOH
";

    assert_eq!(solve_for(input).unwrap(), "Part 1: | Part 2: 3");
}

#[test]
fn test_example2() {
    let input = r"
e => H
e => O
H => HO
H => OH
O => HH

HOHOHO
";

    assert_eq!(solve_for(input).unwrap(), "Part 1: | Part 2: 6");
}
