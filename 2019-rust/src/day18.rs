use std::collections::HashMap;

use crate::util::*;
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 1)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);
    let nodes: HashMap<char, CharGridIndexRC> = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| c.is_ascii_alphabetic() || c == &'@')
        .map(|(p, c)| (c, p))
        .collect();

    dbg!(&nodes);
    let mut steps = 0;

    Ok(format!("steps: {}", steps))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
#########
#b.A.@.a#
#########
"###;
    let result = solve_for(input)?;

    assert_eq!("steps: 8", result);
    Ok(())
}
