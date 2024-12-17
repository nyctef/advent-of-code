use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 17)?;

    let (part1, part2) = solve_for(&input)?;

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
                let denom = 2_usize.pow(self.combo_operand(operand) as u32);
                self.a = self.a / denom;
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
                self.output.push((self.combo_operand(operand) & 0b111) as u8);
                self.pc += 2;
            }
            6 => {
                let denom = 2_usize.pow(self.combo_operand(operand) as u32);
                self.b = self.a / denom;
                self.pc += 2;
            }
            7 => {
                let denom = 2_usize.pow(self.combo_operand(operand) as u32);
                self.c = self.a / denom;
                self.pc += 2;
            }
            _ => panic!("unknown opcode {}", opcode)
        }
    }

    fn print_output(&self) -> String {
        self.output.iter().join(",")
    }

    fn output_is_program(&self) -> bool {
        (&self.output[..]).eq(self.program)
    }
}

fn solve_for(input: &str) -> Result<(String, usize)> {
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

    let mut part2 = 0;
    for i in 1_000_000_000..10_000_000_000 {
        if i % 1_000_000 == 0 {
            eprint!(".");
        }
        let mut octcode = Octcode {
            program: &program,
            pc: 0,
            a: i,
            b: registers[1],
            c: registers[2],
            output: vec![],
        };

        while !octcode.is_stopped() {
            octcode.step();
        }

        if octcode.output_is_program() {
            part2 = i;
            break;
        }
    }

    let part1 = octcode.print_output();
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"###;
    let (part1, _) = solve_for(input)?;

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
    let (_, part2) = solve_for(input)?;

    assert_eq!(part2, 117440);
    Ok(())

}

