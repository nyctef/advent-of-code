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

    let _map = map.iter().join("");

    todo!()
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("expected", result);
    Ok(())
}
