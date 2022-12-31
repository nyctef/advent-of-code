use std::collections::{HashMap, HashSet};

use crate::{aoc_util::*, intcode::IntCode};
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 13)?;

    let mut machine = input.parse::<IntCode>()?;
    machine.set_value_at_position(0, 2);
    machine.run()?;

    let mut output = vec![];
    while let Some(x) = machine.read_output() {
        output.push(x)
    }

    let tiles = output.iter().tuples::<(_, _, _)>().collect_vec();
    let tiles = tiles
        .iter()
        .map(|(x, y, t)| ((**x, **y), t))
        .collect::<HashMap<_, _>>();
    let max_x = tiles.keys().map(|k| k.0).max().unwrap();
    let max_y = tiles.keys().map(|k| k.1).max().unwrap();
    dbg!(max_x, max_y);

    for row in 0..=max_y {
        for col in 0..=max_x {
            print!(
                "{}",
                match tiles.get(&(col, row)) {
                    Some(1) => "█",
                    Some(2) => "#",
                    Some(3) => "_",
                    Some(4) => "o",
                    _other => " ",
                }
            );
        }
        println!();
    }

    println!("Score: {}", tiles[&(-1, 0)]);
    Ok(())
}

#[test]
fn test1() {
    // ...
}
