use std::collections::HashSet;

use crate::{aoc_util::*, intcode::IntCode};
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 13)?;

    let mut machine = input.parse::<IntCode>()?;
    machine.run()?;

    let mut output = vec![];
    while let Some(x) = machine.read_output() {
        output.push(x)
    }

    let tiles = output.iter().tuples::<(_, _, _)>().collect_vec();
    let block_tiles = tiles
        .iter()
        .filter(|(_x, _y, t)| **t == 2)
        .map(|(x, y, _)| (x, y))
        .collect::<HashSet<_>>();
    println!("{}", block_tiles.len());

    Ok(())
}

#[test]
fn test1() {
    // ...
}
