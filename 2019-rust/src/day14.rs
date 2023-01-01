use crate::aoc_util::*;
use color_eyre::{eyre::Result, Report};
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::{all_consuming, map},
    multi::separated_list1,
    sequence::separated_pair,
    Finish,
};

pub fn solve() -> Result<()> {
    let input = get_input(2019, 1)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let parsed = parse_input(input)?;

    todo!()
}

fn parse_input(input: &str) -> Result<Vec<Recipe>> {
    let parse_ingredient = map(separated_pair(digit1, tag(" "), alpha1), Ingredient::from);
    let parse_inputs = separated_list1(tag(", "), parse_ingredient);
    let parse_line = map(
        separated_pair(parse_inputs, tag(" => "), parse_ingredient),
        Recipe::from,
    );
    let parse_lines = separated_list1(tag("\n"), parse_line);
    Ok(all_consuming(parse_lines)(input).finish()?.1)
}

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

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("expected", result);
    Ok(())
}
