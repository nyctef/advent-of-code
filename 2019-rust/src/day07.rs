use itertools::Itertools;

use crate::aoc_util::*;
use crate::err_util::*;
use crate::intcode::IntCode;
use crate::intcode::TInt;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 7)?;

    let intcode = input.parse::<IntCode>()?;

    let phase_combinations = (0..5).permutations(5);

    let result = phase_combinations
        .map(|phase| run_amplifiers(&intcode, phase))
        .max();
    print!("{}", result.unwrap());

    Ok(())
}

fn run_amplifiers(intcode: &IntCode, phases: Vec<TInt>) -> TInt {
    let mut signal = 0;
    for phase_setting in phases {
        let mut machine = intcode.clone();
        machine.queue_input(phase_setting);
        machine.queue_input(signal);

        machine.run().unwrap();

        signal = machine.read_output().unwrap();
    }
    signal
}

#[test]
fn test1() -> Result<()> {
    let machine1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0".parse::<IntCode>()?;
    let result1 = run_amplifiers(&machine1, vec![4, 3, 2, 1, 0]);
    assert_eq!(43210, result1);

    Ok(())
}
