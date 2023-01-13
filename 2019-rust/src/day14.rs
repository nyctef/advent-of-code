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
    collections::{HashMap, HashSet},
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

    // check that we dont have to do anything complicated like choosing between
    // two alternate versions of a recipe
    assert!(
        lookup.len() == parsed.len(),
        "each item should only have one recipe"
    );

    let mut requirements: HashMap<&str, u64> = HashMap::new();
    requirements.insert("FUEL", 1);

    // do a topological sort of the recipes. This means that we won't attempt to process
    // an ingredient until we've processed all the recipes which consume that ingredient.
    // This is important since the math below only works if we process each ingredient
    // exactly once: otherwise we'd need to make the logic more complicated by tracking
    // spare materials left and reusing them when revising ingredients.
    let steps = sort_steps(&lookup);

    for &next in &steps {
        if next == "ORE" {
            continue;
        }

        let recipe = lookup
            .get(next)
            .ok_or(format!("missing recipe for {}", next))
            .map_err(Report::msg)?;
        let needed = requirements.get(next).expect("requirements");
        // we need to run the recipe enough times to give us the required quantity *or more*
        let recipe_runs = div_ceil(*needed, recipe.output.quantity);

        // save the results of running this recipe, then loop on to the next one
        for input in &recipe.inputs {
            *requirements.entry(&input.name).or_insert(0) += input.quantity * recipe_runs;
        }
    }

    dbg!(
        requirements["ORE"],
        1000000000000_i64 - requirements["ORE"] as i64
    );
    Ok(requirements["ORE"].to_string())
}

fn sort_steps<'input_string>(
    lookup: &HashMap<&'input_string str, &'input_string Recipe>,
) -> Vec<&'input_string str> {
    // topological sort of recipes: a rough implementation of Kahn's algorithm
    // https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm

    // gather all the edges in the recipe graph for easy lookup
    let mut edges: HashSet<(&str, &str)> = lookup
        .values()
        .flat_map(|r| {
            r.inputs
                .iter()
                .map(|i| (r.output.name.as_ref(), i.name.as_ref()))
        })
        .collect::<HashSet<_>>();

    let mut q: Vec<&str> = vec![];
    // we happen to know that FUEL is one node in the graph with no
    // incoming edges (nothing consumes it)
    q.push(lookup["FUEL"].output.name.as_ref());
    let mut result = vec![];

    while let Some(next_output) = q.pop() {
        // we now know that `next` has no surviving incoming edges, so
        // we can safely put it next in the resulting order
        result.push(next_output);

        // collect all edges going out from `next`...
        let edges_from_next = edges
            .iter()
            .filter(|(o, _)| *o == next_output)
            .copied()
            .collect_vec();
        // ...and remove them from our collection
        edges.retain(|(o, _)| *o != next_output);

        // After removing the edges going out from `next`, if any of
        // the referenced recipes no longer have incoming edges, we can
        // process those next.
        for (_, candidate) in edges_from_next {
            if !edges.iter().any(|(_, i)| *i == candidate) {
                q.push(candidate);
            }
        }
    }
    result
}

fn div_ceil(a: u64, b: u64) -> u64 {
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
    quantity: u64,
    name: String,
}
impl Ingredient {
    fn from((q, n): (&str, &str)) -> Self {
        Self {
            quantity: q.parse::<u64>().unwrap(),
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
