use crate::aoc_util::*;
use color_eyre::{eyre::Result, Report};
use itertools::Itertools;
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::{eof, map, opt},
    error::convert_error,
    multi::{many_till, separated_list1},
    sequence::{separated_pair, terminated},
    Finish,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
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
    let steps = sort_steps(&lookup);
    // dbg!(&steps);

    for &next in &steps {
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

        // dbg!(next, recipe, needed, recipe_runs);
        // println!("-----------------------------");

        for input in &recipe.inputs {
            *requirements.entry(&input.name).or_insert(0) += input.quantity * recipe_runs;
        }
    }

    Ok(requirements["ORE"].to_string())
}

fn sort_steps<'input_string>(
    lookup: &HashMap<&'input_string str, &'input_string Recipe>,
) -> Vec<&'input_string str> {
    // list of edges (output -> input)
    let mut edges: HashSet<(&str, &str)> = lookup
        .values()
        .flat_map(|r| {
            r.inputs
                .iter()
                .map(|i| (r.output.name.as_ref(), i.name.as_ref()))
        })
        .collect::<HashSet<_>>();

    let mut q: Vec<&str> = vec![];
    q.push(lookup["FUEL"].output.name.as_ref());
    let mut result = vec![];

    while let Some(nextOutput) = q.pop() {
        result.push(nextOutput);
        // remove all edges
        let edges_from_next = edges
            .iter()
            .filter(|(o, i)| *o == nextOutput)
            .copied()
            .collect_vec();
        edges.retain(|(o, i)| *o != nextOutput);
        // dbg!(&edges_from_next, &edges);
        for (_, candidate) in edges_from_next {
            if !edges.iter().any(|(_, i)| *i == candidate) {
                // candidate has no remaining incoming edges
                q.push(candidate);
            }
        }
    }
    result
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
    let parse_line_with_ending = terminated(parse_line, opt(tag("\n")));
    let result = many_till(parse_line_with_ending, eof)(input.trim())
        .finish()
        .map_err(|e| Report::msg(convert_error(input, e)))?
        .1
         .0
        .into_iter()
        .collect_vec();
    Ok(result)
}

#[derive(Debug, PartialEq, Eq, Hash)]
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

#[derive(PartialEq, Eq, Hash)]
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
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
"###;
    let result = solve_for(input)?;

    assert_eq!("31", result);
    Ok(())
}
