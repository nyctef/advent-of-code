use itertools::Itertools;

use crate::util::*;
use crate::intcode::IntCode;
use crate::intcode::MachineState;
use crate::intcode::TInt;
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 7)?;

    let intcode = input.parse::<IntCode>()?;

    let phase_combinations = (5..10).permutations(5);

    let result = phase_combinations
        .map(|phase| run_amplifiers(&intcode, phase))
        .max();
    print!("{}", result.unwrap());

    Ok(())
}

fn run_amplifiers(intcode: &IntCode, phases: Vec<TInt>) -> TInt {
    let mut machines: Vec<_> = phases
        .into_iter()
        .map(|p| {
            let mut machine = intcode.clone();
            machine.queue_input(p);
            machine
        })
        .collect();

    let mut current_machine_index = 0;
    // machines take input from `current_machine_index` and output to `current_machine_index + 1`
    let mut signals: Vec<TInt> = vec![0; machines.len()];
    loop {
        let next_machine_index = (current_machine_index + 1) % machines.len();
        let machine = &mut machines[current_machine_index];
        machine.queue_input(signals[current_machine_index]);

        // dbg!(&current_machine_index, &signals, next_machine_index);
        machine.run().unwrap();
        // dbg!(machine.state());

        if let Some(output) = machine.read_output() {
            signals[next_machine_index] = output;
        }
        if machine.state().1 == MachineState::Halted && current_machine_index == machines.len() - 1
        {
            // stopped at the last machine, so we must have our answer
            break;
        } else {
            current_machine_index = next_machine_index;
        }
    }
    signals[0]
}

#[test]
fn test_part1() -> Result<()> {
    let machine1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0".parse::<IntCode>()?;
    let result1 = run_amplifiers(&machine1, vec![4, 3, 2, 1, 0]);
    assert_eq!(43210, result1);
    Ok(())
}

#[test]
fn test_part2() -> Result<()> {
    let machine2 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
        .parse::<IntCode>()?;
    machine2.print_memory();
    let result2 = run_amplifiers(&machine2, vec![9, 8, 7, 6, 5]);
    assert_eq!(139629729, result2);
    Ok(())
}
