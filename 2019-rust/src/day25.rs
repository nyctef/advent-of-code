use crate::{intcode::IntCode, util::*};
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::io::stdin;
use std::str::FromStr;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 25)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut intcode = IntCode::from_str(input)?;

    let mut line_buffer = String::new();
    for instr in [
        "north",
        "take sand",
        "north",
        "take space heater",
        "east",
        "take semiconductor",
        "west",
        "south",
        "south",
        "east",
        "take ornament",
        "south",
        // science lab
        "take festive hat",
        "east",
        // storage
        "take asterisk",
        "south",
        // navigation
        "east",
        // kitchen
        "take cake",
        "west",
        // navigation
        "west",
        // holodeck
        "take food ration",
        // to the checkpoint!
        "east",
        "north",
        "west",
        "north",
        "west",
        "west",
        "north",
        "north",
    ] {
        for c in instr.chars() {
            intcode.queue_input(c as i64);
        }
        intcode.queue_input('\n' as i64);
    }

    let stuff = [
        "sand",
        "food ration",
        "cake",
        "asterisk",
        "festive hat",
        "ornament",
        "semiconductor",
        "space heater",
    ];
    'security: for choice_len in 1..=8 {
        for choice in stuff.iter().combinations(choice_len) {
            println!("ATTEMPTING: {:?}", choice);

            //reset first
            for s in stuff {
                for c in format!("drop {s}\n").chars() {
                    intcode.queue_input(c as i64);
                }
            }

            for s in choice {
                for c in format!("take {s}\n").chars() {
                    intcode.queue_input(c as i64);
                }
            }

            for c in "west\n".chars() {
                intcode.queue_input(c as i64);
            }

            intcode.run()?;

            let mut response = String::new();
            while let Some(x) = intcode.read_output() {
                response.push(x as u8 as char);
            }

            if !response.contains("you are ejected") {
                println!("{}", response);
                break 'security;
            }
        }
    }

    loop {
        intcode.run()?;

        while let Some(x) = intcode.read_output() {
            print!("{}", x as u8 as char);
        }

        line_buffer.clear();
        stdin().read_line(&mut line_buffer)?;
        for c in line_buffer.chars() {
            intcode.queue_input(c as i64);
        }
    }
}
