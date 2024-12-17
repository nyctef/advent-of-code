use std::{collections::VecDeque, u64};

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 17)?;

    let (part1, part2) = solve_for(&input, true)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Debug, Clone)]
struct Octcode<'p> {
    program: &'p [u8],
    pc: usize,
    a: usize,
    b: usize,
    c: usize,
    output: Vec<u8>,
}

impl<'p> Octcode<'p> {
    fn is_stopped(&self) -> bool {
        self.pc + 1 >= self.program.len()
    }

    fn combo_operand(&self, operand: u8) -> usize {
        match operand {
            0..=3 => operand as usize,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            7 => panic!("reserved!"),
            _ => panic!("unknown combo operand {}", operand),
        }
    }

    fn step(&mut self) {
        if self.is_stopped() {
            panic!("is stopped");
        }

        let opcode = self.program[self.pc];
        let operand = self.program[self.pc + 1];

        // eprintln!("pc {} oc {} oa {}", self.pc, opcode, operand);

        match opcode {
            0 => {
                self.a = self.a >> self.combo_operand(operand);
                self.pc += 2;
            }
            1 => {
                self.b = self.b ^ operand as usize;
                self.pc += 2;
            }
            2 => {
                self.b = self.combo_operand(operand) & 0b111;
                self.pc += 2;
            }
            3 => {
                if self.a != 0 {
                    self.pc = operand as usize;
                } else {
                    self.pc += 2;
                }
            }
            4 => {
                self.b = self.b ^ self.c;
                self.pc += 2;
            }
            5 => {
                self.output
                    .push((self.combo_operand(operand) & 0b111) as u8);
                self.pc += 2;
            }
            6 => {
                self.b = self.a >> self.combo_operand(operand);
                self.pc += 2;
            }
            7 => {
                self.c = self.a >> self.combo_operand(operand);
                self.pc += 2;
            }
            _ => panic!("unknown opcode {}", opcode),
        }
    }

    fn print_output(&self) -> String {
        self.output.iter().join(",")
    }

    fn get_output(&self) -> &[u8] {
        &self.output
    }

    fn output_is_program(&self) -> bool {
        (&self.output[..]).eq(self.program)
    }

    fn reset(&mut self, a: usize, b: usize, c: usize) {
        self.output.clear();
        self.pc = 0;
        self.a = a;
        self.b = b;
        self.c = c;
    }
}

fn solve_for(input: &str, do_part2: bool) -> Result<(String, usize)> {
    let (registers, program) = input.trim().split_once("\n\n").unwrap();

    let registers = registers
        .lines()
        .map(|l| all_numbers_usize(l)[0])
        .collect_vec();
    let program = all_numbers_u8(program);

    let mut octcode = Octcode {
        program: &program,
        pc: 0,
        a: registers[0],
        b: registers[1],
        c: registers[2],
        output: vec![],
    };

    while !octcode.is_stopped() {
        octcode.step();
    }
    let part1 = octcode.print_output();
    if !do_part2 {
        return Ok((part1, 0));
    }

    let mut queue = VecDeque::new();
    // target_len, a
    queue.push_front((1, 0_usize));
    let mut best_solution = usize::MAX;

    while let Some((target_len, mut a)) = queue.pop_front() {
        if target_len > program.len() {
            eprintln!("found a full solution {}", a);
            if a < best_solution {
                best_solution = a;
            }
            continue;
        }

        a <<= 3;
        let target = &program[program.len() - target_len..program.len()];
        eprintln!("a: {} | trying to match {:?}", a, target);
        for next_a_part in 0..=7 {
            if a == 0 && next_a_part == 0 {
                // we can't start with a zero, because otherwise we'd have quit on the previous iteration
                continue;
            }
            octcode.reset(a + next_a_part, registers[1], registers[2]);
            while !octcode.is_stopped() {
                octcode.step();
            }

            if (octcode.get_output()).eq(target) {
                eprintln!(
                    "found match: {}  {{ {:?} == {:?} }}",
                    next_a_part,
                    octcode.get_output(),
                    target
                );
                queue.push_front((target_len + 1, a + next_a_part));
            }
        }
    }

    Ok((part1, best_solution))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"###;
    let (part1, _) = solve_for(input, false)?;

    assert_eq!(part1, "4,6,3,5,6,3,5,2,1,0");
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
"###;
    let (_, part2) = solve_for(input, true)?;

    assert_eq!(part2, 117440);
    Ok(())
}
