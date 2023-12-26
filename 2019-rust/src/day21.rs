use std::str::FromStr;
use crate::{intcode::IntCode, util::*};
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 21)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut intcode = IntCode::from_str(input)?;

    for c in "NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
".chars() {
        intcode.queue_input(c as i64);
    }

    intcode.run()?;

    while let Some(x) = intcode.read_output() {
        if x > 255 {
            return Ok(format!("{}", x));
        }
        let c = x as u8 as char;
        print!("{}", c);
    }
    Ok("try again".to_owned())
}
