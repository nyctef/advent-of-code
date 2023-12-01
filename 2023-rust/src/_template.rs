use crate::aoc_util::*;
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 1)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    // ...

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
