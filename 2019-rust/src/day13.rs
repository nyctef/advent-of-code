use crate::{
    util::*,
    intcode::{IntCode, MachineState},
};
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::collections::HashMap;

type Screen = HashMap<(i64, i64), i64>;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 13)?;

    let mut screen = Screen::new();
    let mut machine = input.parse::<IntCode>()?;
    machine.set_value_at_position(0, 2);

    loop {
        machine.run()?;

        update_screen(&mut machine, &mut screen);
        draw_screen(&screen);

        // thread::sleep(time::Duration::from_millis(50));

        if machine.state().1 == MachineState::Halted {
            break;
        } else if machine.state().1 == MachineState::AwaitingInput {
            let paddle_col = screen.iter().find(|t| *t.1 == 3).unwrap().0 .0;
            let ball_col = screen.iter().find(|t| *t.1 == 4).unwrap().0 .0;
            machine.queue_input(match paddle_col.cmp(&ball_col) {
                std::cmp::Ordering::Less => 1,
                std::cmp::Ordering::Equal => 0,
                std::cmp::Ordering::Greater => -1,
            });
        }
    }

    Ok(())
}

fn update_screen(machine: &mut IntCode, screen: &mut Screen) {
    // https://stackoverflow.com/questions/34837011/
    print!("{esc}[2J{esc}[1;1H", esc = 27 as char);

    let mut output = vec![];
    while let Some(x) = machine.read_output() {
        output.push(x)
    }
    output
        .iter()
        .tuples::<(_, _, _)>()
        .collect_vec()
        .iter()
        .for_each(|(x, y, t)| {
            screen.insert((**x, **y), **t);
        });
}

fn draw_screen(screen: &Screen) {
    let max_x = screen.keys().map(|k| k.0).max().unwrap();
    let max_y = screen.keys().map(|k| k.1).max().unwrap();
    dbg!(max_x, max_y);
    for row in 0..=max_y {
        for col in 0..=max_x {
            print!(
                "{}",
                match screen.get(&(col, row)) {
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
    println!("Score: {:?}", screen.get(&(-1, 0)));
}

#[test]
fn test1() {
    // ...
}
