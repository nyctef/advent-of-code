use crate::aoc_util::*;
use crate::intcode::IntCode;
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 9)?;
    let mut boost = input.parse::<IntCode>()?;
    boost.queue_input(2);

    boost.run()?;

    let mut output = vec![];
    while let Some(o) = boost.read_output() {
        output.push(o);
    }

    dbg!(output);
    Ok(())
}

#[test]
fn test_quine() {
    let mut machine = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        .parse::<IntCode>()
        .unwrap();
    machine.run().unwrap();
    let mut output = vec![];
    while let Some(o) = machine.read_output() {
        output.push(o);
    }
    assert_eq!(
        vec![109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99],
        output
    );
}
