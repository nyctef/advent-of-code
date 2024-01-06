use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::fmt::Display;

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
    let nonterminals = replacements
        .iter()
        .map(|(r, _)| Term::Nonterminal(r))
        .collect_vec();
    // println!("nonterminals {nonterminals:?}");
    let mut rules = replacements
        .iter()
        .map(|(r, e)| {
            Rule::new(
                Term::Nonterminal(r),
                e.iter()
                    .map(
                        |t| match nonterminals.iter().find(|nt| nt.get_value() == *t) {
                            // if we find a matching element in `nonterminals`, we could just
                            // return that, but we need to return something owned here so that
                            // both match arms return the same type of value
                            Some(_) => Term::Nonterminal(t),
                            None => Term::Terminal(t),
                        },
                    )
                    .collect_vec(),
            )
        })
        .collect_vec();

    // every nonterminal here can also end up as an equivalent terminal symbol,
    // so we need to add those rules
    rules.extend(
        rules
            .iter()
            .flat_map(|r| &r.expansion)
            .filter(|e| matches!(e, Term::Nonterminal(_)))
            .unique()
            .map(|nt| Rule::new(nt.clone(), vec![Term::Terminal(nt.get_value())]))
            .collect_vec(),
    );

    dbg!(rules);
    let _target = tokenize(target);

    // println!("r {:?} t {:?}", replacements, target);

    // solve(&replacements, target);

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

#[derive(Eq, PartialEq, Hash, Clone)]
enum Term<'i> {
    Terminal(&'i str),
    Nonterminal(&'i str),
}

impl Display for Term<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Terminal(t) => f.write_fmt(format_args!("'{}'", t)),
            Term::Nonterminal(nt) => f.write_fmt(format_args!("<{}>", nt)),
        }
    }
}

impl std::fmt::Debug for Term<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.to_string()))
    }
}

impl<'i> Term<'i> {
    fn get_value(&self) -> &'i str {
        match self {
            Term::Terminal(t) => t,
            Term::Nonterminal(nt) => nt,
        }
    }
}

#[derive(Eq, PartialEq, Constructor)]
struct Rule<'i> {
    matches: Term<'i>,
    expansion: Vec<Term<'i>>,
}

impl std::fmt::Debug for Rule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?} => {:?}", self.matches, self.expansion))
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
