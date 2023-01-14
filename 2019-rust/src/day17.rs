use std::collections::HashMap;

use crate::{aoc_util::*, intcode::IntCode};
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 17)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut machine = input.parse::<IntCode>()?;
    machine.run()?;
    let mut map = vec![];
    while let Some(c) = machine.read_output() {
        map.push(c as u8 as char);
    }

    let map = map.iter().join("");

    println!("{}", &map);

    let scaffolds = map
        .lines()
        .enumerate()
        .map(|(r, l)| {
            l.chars()
                .enumerate()
                .map(move |(c, char)| ((r as i32, c as i32), char))
        })
        .flatten()
        .filter(|(_p, c)| c == &'#')
        .collect::<HashMap<_, _>>();

    let mut align_param_sum = 0;
    for (p, _) in &scaffolds {
        // if north, south, east and west are also in the collection, then this is an intersection
        if scaffolds.contains_key(&(p.0 + 1, p.1))
            && scaffolds.contains_key(&(p.0 - 1, p.1))
            && scaffolds.contains_key(&(p.0, p.1 + 1))
            && scaffolds.contains_key(&(p.0, p.1 - 1))
        {
            align_param_sum += p.0 * p.1;
        }
    }

    Ok(format!("{}", align_param_sum))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("expected", result);
    Ok(())
}
