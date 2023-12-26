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

    let instructions = [
        // jump if gap one steps ahead
        "NOT A J",
        // jump if gap two steps ahead
        "NOT B T\nOR T J",
        // jump if gap three steps ahead
        "NOT C T\nOR T J",
        // only jump if will land on ground
        "AND D J",
        // only jump if we can 1. jump immediately or 2. go forward
        // after landing
        "AND H T\nOR E T\nAND T J",
        "RUN"

    ];
    for c in instructions.join("\n").chars() {
        intcode.queue_input(c as i64);
    }
    intcode.queue_input('\n' as i64);

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
