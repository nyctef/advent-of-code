use crate::aoc_util::*;
use crate::err_util::*;
use crate::intcode::IntCode;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 5)?;

    let mut intcode = input.parse::<IntCode>()?;
    intcode.queue_input(1);
    intcode.run()?;
    while let Some(o) = intcode.read_output() {
        dbg!(o);
    }

    Ok(())
}

#[test]
fn test1() {
    // ...
}
