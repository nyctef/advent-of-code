use std::str::FromStr;

use crate::aoc_util::*;
use crate::err_util::*;

// TODO: can we actually make IntCode generic on the size of the integer?
type TInt = usize;

#[derive(Debug)]
struct IntCode {
    data: Vec<TInt>,
}

#[derive(Debug)]
enum Opcode {
    Add {
        input_pos_1: usize,
        input_pos_2: usize,
        output_pos: usize,
    },
    Mul {
        input_pos_1: usize,
        input_pos_2: usize,
        output_pos: usize,
    },
    Halt,
}

impl IntCode {
    /// Overwrites the value at position (zero-indexed)
    fn set_value_at_position(&mut self, position: usize, value: TInt) {
        self.data[position] = value;
    }

    fn get_value_at(&self, position: usize) -> TInt {
        self.data[position]
    }

    fn read_op_at(&self, position: &mut usize) -> Result<Opcode> {
        let opnum = self.data[*position];
        match opnum {
            1 => {
                let op = Opcode::Add {
                    input_pos_1: self.data[*position + 1],
                    input_pos_2: self.data[*position + 2],
                    output_pos: self.data[*position + 3],
                };
                *position += 4;
                return Ok(op);
            }
            2 => {
                let op = Opcode::Mul {
                    input_pos_1: self.data[*position + 1],
                    input_pos_2: self.data[*position + 2],
                    output_pos: self.data[*position + 3],
                };
                *position += 4;
                return Ok(op);
            }
            99 => {
                let op = Opcode::Halt;
                *position += 1;
                return Ok(op);
            }
            other => todo!("unknown opcode {other}"),
        };
    }

    fn execute_op(&mut self, op: Opcode) -> bool {
        match op {
            Opcode::Add {
                input_pos_1,
                input_pos_2,
                output_pos,
            } => {
                let a = self.data[input_pos_1];
                let b = self.data[input_pos_2];
                self.data[output_pos] = a + b;
                return true;
            }
            Opcode::Mul {
                input_pos_1,
                input_pos_2,
                output_pos,
            } => {
                let a = self.data[input_pos_1];
                let b = self.data[input_pos_2];
                self.data[output_pos] = a * b;
                return true;
            }
            Opcode::Halt => return false,
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
        Ok(IntCode { data: nums })
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
        let op = intcode.read_op_at(&mut pc)?;
        running = intcode.execute_op(op);
    }

    print!("{}", intcode.get_value_at(0));

    Ok(())
}

#[test]
fn test1() {
    // ...
}
