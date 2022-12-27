use std::str::FromStr;

use crate::aoc_util::*;
use crate::err_util::*;

// TODO: can we actually make IntCode generic on the size of the integer?
type TInt = usize;

#[derive(Debug)]
struct IntCode {
    memory: Vec<TInt>,
}

#[derive(Debug)]
enum Instruction {
    Add {
        input_addr_1: usize,
        input_addr_2: usize,
        output_addr: usize,
    },
    Mul {
        input_addr_1: usize,
        input_addr_2: usize,
        output_addr: usize,
    },
    Halt,
}

impl IntCode {
    /// Overwrites the value at position (zero-indexed)
    fn set_value_at_position(&mut self, position: usize, value: TInt) {
        self.memory[position] = value;
    }

    fn get_value_at(&self, position: usize) -> TInt {
        self.memory[position]
    }

    fn read_instr_at(&self, position: &mut usize) -> Result<Instruction> {
        let opnum = self.memory[*position];
        match opnum {
            1 => {
                let instr = Instruction::Add {
                    input_addr_1: self.memory[*position + 1],
                    input_addr_2: self.memory[*position + 2],
                    output_addr: self.memory[*position + 3],
                };
                *position += 4;
                return Ok(instr);
            }
            2 => {
                let instr = Instruction::Mul {
                    input_addr_1: self.memory[*position + 1],
                    input_addr_2: self.memory[*position + 2],
                    output_addr: self.memory[*position + 3],
                };
                *position += 4;
                return Ok(instr);
            }
            99 => {
                let instr = Instruction::Halt;
                *position += 1;
                return Ok(instr);
            }
            other => todo!("unknown opcode {other}"),
        };
    }

    fn execute_instr(&mut self, instr: Instruction) -> bool {
        match instr {
            Instruction::Add {
                input_addr_1,
                input_addr_2,
                output_addr,
            } => {
                let a = self.memory[input_addr_1];
                let b = self.memory[input_addr_2];
                self.memory[output_addr] = a + b;
                return true;
            }
            Instruction::Mul {
                input_addr_1,
                input_addr_2,
                output_addr,
            } => {
                let a = self.memory[input_addr_1];
                let b = self.memory[input_addr_2];
                self.memory[output_addr] = a * b;
                return true;
            }
            Instruction::Halt => return false,
        }
    }
}

impl FromStr for IntCode {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self> {
        let nums = s
            .trim()
            .split(",")
            .map(|x| TInt::from_str_radix(x, 10))
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(|_e| "failed to parse input as a number")?;
        Ok(IntCode { memory: nums })
    }
}

pub fn solve() -> Result<()> {
    let input = get_input(2019, 2)?;

    let mut intcode: IntCode = input.parse()?;
    intcode.set_value_at_position(1, 12);
    intcode.set_value_at_position(2, 2);

    let mut pc: usize = 0;

    let mut running = true;
    while running {
        let instr = intcode.read_instr_at(&mut pc)?;
        running = intcode.execute_instr(instr);
    }

    print!("{}", intcode.get_value_at(0));

    Ok(())
}

#[test]
fn test1() {
    // ...
}
