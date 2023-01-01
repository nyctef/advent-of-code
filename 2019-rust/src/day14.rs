use crate::aoc_util::*;
use color_eyre::{eyre::Result, Report};
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::{all_consuming, map},
    error::{convert_error, ErrorKind, VerboseError},
    multi::separated_list1,
    sequence::separated_pair,
    Finish,
};
use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
};

pub fn solve() -> Result<()> {
    let input = get_input(2019, 14)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let parsed = parse_input(input)?;

    let lookup = parsed
        .iter()
        .map(|r| (&r.output.name[..], r))
        .collect::<HashMap<_, _>>();

    assert!(
        lookup.len() == parsed.len(),
        "each item should only have one recipe"
    );

    let mut requirements: HashMap<&str, u32> = HashMap::new();
    requirements.insert("FUEL", 1);
    let mut steps: VecDeque<&str> = VecDeque::new();
    steps.push_back("FUEL");

    while let Some(next) = steps.pop_front() {
        if next == "ORE" {
            continue;
        }
        // in theory we should have calculated the total required sum of this ingredient by now
        let recipe = lookup
            .get(next)
            .ok_or(format!("missing recipe for {}", next))
            .map_err(Report::msg)?;
        let needed = requirements.get(next).expect("requirements");
        let recipe_runs = div_ceil(*needed, recipe.output.quantity);

        dbg!(next, recipe, needed, recipe_runs);
        println!("-----------------------------");

        for input in &recipe.inputs {
            *requirements.entry(&input.name).or_insert(0) += input.quantity * recipe_runs;
            if !steps.contains(&&input.name[..]) {
                steps.push_back(&input.name);
            }
        }
    }

    Ok(requirements["ORE"].to_string())
}

fn div_ceil(a: u32, b: u32) -> u32 {
    // https://stackoverflow.com/a/72442854/895407
    (a + b - 1) / b
}

fn parse_input(input: &str) -> Result<Vec<Recipe>> {
    let parse_ingredient = || map(separated_pair(digit1, tag(" "), alpha1), Ingredient::from);
    let parse_inputs = separated_list1(tag(", "), parse_ingredient());
    let parse_line = map(
        separated_pair(parse_inputs, tag(" => "), parse_ingredient()),
        Recipe::from,
    );
    let parse_lines = separated_list1(tag("\n"), parse_line);
    let result = all_consuming::<_, _, VerboseError<_>, _>(parse_lines)(input.trim())
        .finish()
        .map_err(|e| Report::msg(convert_error(input, e)));
    Ok(result?.1)
}

#[derive(Debug)]
struct Recipe {
    inputs: Vec<Ingredient>,
    output: Ingredient,
}
impl Recipe {
    fn from((i, o): (Vec<Ingredient>, Ingredient)) -> Self {
        Self {
            inputs: i,
            output: o,
        }
    }
}

struct Ingredient {
    quantity: u32,
    name: String,
}
impl Ingredient {
    fn from((q, n): (&str, &str)) -> Self {
        Self {
            quantity: u32::from_str_radix(q, 10).unwrap(),
            name: n.to_owned(),
        }
    }
}
impl Debug for Ingredient {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.quantity, self.name)
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
10 ORE => 10 A
--1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
"###;
    let result = solve_for(input)?;

    assert_eq!("31", result);
    Ok(())
}
