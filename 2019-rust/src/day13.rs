use crate::{
    aoc_util::*,
    intcode::{IntCode, MachineState},
};
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::time;
use std::{collections::HashMap, thread};

pub fn solve() -> Result<()> {
    let input = get_input(2019, 13)?;

    let mut machine = input.parse::<IntCode>()?;
    machine.set_value_at_position(0, 2);

    loop {
        machine.run()?;

        print_game(&mut machine);

        thread::sleep(time::Duration::from_millis(500));

        if machine.state().1 == MachineState::Halted {
            break;
        } else if machine.state().1 == MachineState::AwaitingInput {
            machine.queue_input(1);
        }
    }

    Ok(())
}

fn print_game(machine: &mut IntCode) {
    // https://stackoverflow.com/questions/34837011/
    print!("{esc}[2J{esc}[1;1H", esc = 27 as char);

    let mut output = vec![];
    while let Some(x) = machine.read_output() {
        output.push(x)
    }
    let tiles = output.iter().tuples::<(_, _, _)>().collect_vec();
    let tiles = tiles
        .iter()
        .map(|(x, y, t)| ((**x, **y), t))
        .collect::<HashMap<_, _>>();
    let max_x = tiles.keys().map(|k| k.0).max().unwrap();
    let max_y = tiles.keys().map(|k| k.1).max().unwrap();
    dbg!(max_x, max_y);
    for row in 0..=max_y {
        for col in 0..=max_x {
            print!(
                "{}",
                match tiles.get(&(col, row)) {
                    Some(1) => "█",
                    Some(2) => "#",
                    Some(3) => "_",
                    Some(4) => "o",
                    _other => " ",
                }
            );
        }
        println!();
    }
    println!("Score: {:?}", tiles.get(&(-1, 0)));
}

#[test]
fn test1() {
    // ...
}
