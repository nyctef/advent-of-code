use crate::err_util::*;
use std::str::FromStr;

// TODO: can we actually make IntCode generic on the size of the integer?
pub type TInt = usize;

#[derive(Debug, Clone)]
pub struct IntCode {
    memory: Vec<TInt>,
}

#[derive(Debug)]
enum Parameter {
    Address(usize),
    Value(TInt),
}

impl Parameter {
    fn get_value(&self, ic: &IntCode) -> TInt {
        match self {
            Parameter::Address(addr) => ic.memory[*addr],
            Parameter::Value(value) => *value,
        }
    }
}

#[derive(Debug)]
enum Instruction {
    Add {
        input_1: Parameter,
        input_2: Parameter,
        output_addr: usize,
    },
    Mul {
        input_1: Parameter,
        input_2: Parameter,
        output_addr: usize,
    },
    Halt,
}

impl IntCode {
    pub fn run(&mut self) -> Result<()> {
        let mut pc: usize = 0;
        let mut running = true;
        while running {
            let instr = self.read_instr_at(&mut pc)?;
            running = self.execute_instr(instr);
        }
        Ok(())
    }

    /// Overwrites the value at position (zero-indexed)
    pub fn set_value_at_position(&mut self, position: usize, value: TInt) {
        self.memory[position] = value;
    }

    pub fn get_value_at(&self, position: usize) -> TInt {
        self.memory[position]
    }

    fn read_instr_at(&self, position: &mut usize) -> Result<Instruction> {
        let opnum = self.memory[*position];
        match opnum {
            1 => {
                let instr = Instruction::Add {
                    input_1: Parameter::Address(self.memory[*position + 1]),
                    input_2: Parameter::Address(self.memory[*position + 2]),
                    output_addr: self.memory[*position + 3],
                };
                *position += 4;
                Ok(instr)
            }
            2 => {
                let instr = Instruction::Mul {
                    input_1: Parameter::Address(self.memory[*position + 1]),
                    input_2: Parameter::Address(self.memory[*position + 2]),
                    output_addr: self.memory[*position + 3],
                };
                *position += 4;
                Ok(instr)
            }
            99 => {
                let instr = Instruction::Halt;
                *position += 1;
                Ok(instr)
            }
            other => todo!("unknown opcode {other}"),
        }
    }

    fn execute_instr(&mut self, instr: Instruction) -> bool {
        match instr {
            Instruction::Add {
                input_1,
                input_2,
                output_addr,
            } => {
                let a = input_1.get_value(self);
                let b = input_2.get_value(self);
                self.memory[output_addr] = a + b;
                true
            }
            Instruction::Mul {
                input_1,
                input_2,
                output_addr,
            } => {
                let a = input_1.get_value(self);
                let b = input_2.get_value(self);
                self.memory[output_addr] = a * b;
                true
            }
            Instruction::Halt => false,
        }
    }
}

impl FromStr for IntCode {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self> {
        let nums = s
            .trim()
            .split(',')
            .map(|x| TInt::from_str_radix(x, 10))
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(|_e| "failed to parse input as a number")?;
        Ok(IntCode { memory: nums })
    }
}
