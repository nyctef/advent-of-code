use std::{collections::HashMap, isize};

use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 23)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Default, Debug, Clone)]
struct Computer {
    a: u64,
    b: u64,
    pc: usize,
}

impl Computer {
    fn get_register(&self, r: &str) -> u64 {
        match r {
            "a" => self.a,
            "b" => self.b,
            _ => panic!("unknown register {}", r),
        }
    }
    fn set_register(&mut self, r: &str, v: u64) {
        match r {
            "a" => self.a = v,
            "b" => self.b = v,
            _ => panic!("unknown register {}", r),
        }
    }
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let program = input
        .trim()
        .lines()
        .map(|l| {
            let (instr, args) = l.split_once(' ').unwrap();
            let args = args.split(", ").collect_vec();
            (instr, args)
        })
        .collect_vec();

    let mut computer = Computer::default();

    while computer.pc < program.len() {
        let (instr, args) = &program[computer.pc];

        match *instr {
            "hlf" => {
                let register = args[0];
                computer.set_register(register, computer.get_register(register) / 2);
                computer.pc += 1;
            }
            "tpl" => {
                let register = args[0];
                computer.set_register(register, computer.get_register(register) * 3);
                computer.pc += 1;
            }
            "inc" => {
                let register = args[0];
                computer.set_register(register, computer.get_register(register) + 1);
                computer.pc += 1;
            }
            "jmp" => {
                let offset = args[0].parse::<isize>().unwrap();
                computer.pc = (computer.pc as isize + offset)
                    .try_into()
                    .expect("can't jump before program start");
            }
            "jie" => {
                let register = args[0];
                let offset = args[1].parse::<isize>().unwrap();
                if computer.get_register(register) % 2 == 0 {
                    computer.pc = (computer.pc as isize + offset)
                        .try_into()
                        .expect("can't jump before program start");
                } else {
                    computer.pc += 1;
                }
            }
            "jio" => {
                let register = args[0];
                let offset = args[1].parse::<isize>().unwrap();
                if computer.get_register(register) == 1 {
                    computer.pc = (computer.pc as isize + offset)
                        .try_into()
                        .expect("can't jump before program start");
                } else {
                    computer.pc += 1;
                }
            }
            _ => panic!("unrecognised instruction {}", instr),
        }
    }

    let part1 = computer.b;
    let part2 = 0;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
inc a
jio a, +2
tpl a
inc a
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 0);
    assert_eq!(part2, 0);
    Ok(())
}
