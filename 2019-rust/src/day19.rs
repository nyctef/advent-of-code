use std::str::FromStr;

use crate::{util::*, intcode::IntCode};
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 19)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let intcode = IntCode::from_str(input)?;

    let mut total = 0;
    for x in 0..50 {
        for y in 0..50 {
            let mut p = intcode.clone();
            p.queue_input(x);
            p.queue_input(y);
            p.run()?;
            let result = p.read_output().unwrap();
            total += result;
        }
    }

    Ok(format!("total: {}", total))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("expected", result);
    Ok(())
}
