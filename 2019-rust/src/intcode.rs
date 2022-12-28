use crate::err_util::*;
use std::str::FromStr;

// TODO: can we actually make IntCode generic on the size of the integer?
pub type TInt = i32;

#[derive(Debug, Clone)]
pub struct IntCode {
    memory: Vec<TInt>,
    input: Vec<TInt>,
    output: Vec<TInt>,
    running: bool,
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
    fn as_output_address(&self, ic: &IntCode) -> usize {
        match self {
            Parameter::Address(x) => *x,
            Parameter::Value(_) => panic!(),
        }
    }

    fn as_address(&self, ic: &IntCode) -> usize {
        self.get_value(ic).try_into().unwrap()
    }

    fn make(param_mode: u8, value: TInt) -> Result<Parameter> {
        let param = match param_mode {
            0 => Parameter::Address(value.try_into().map_err(|e| {
                format!(
                    "Failed to convert value {value:?} to a usize to use as a memory address: {e}"
                )
            })?),
            1 => Parameter::Value(value),
            other => todo!("Unknown param_mode {other}"),
        };
        Ok(param)
    }
}

#[derive(Debug)]
enum Instruction {
    Add {
        input_1: Parameter,
        input_2: Parameter,
        output_addr: Parameter,
    },
    Mul {
        input_1: Parameter,
        input_2: Parameter,
        output_addr: Parameter,
    },
    Input {
        output_addr: Parameter,
    },
    Output {
        input: Parameter,
    },
    JumpNZ {
        input: Parameter,
        target: Parameter,
    },
    JumpZ {
        input: Parameter,
        target: Parameter,
    },
    LT {
        input_1: Parameter,
        input_2: Parameter,
        output_addr: Parameter,
    },
    Eq {
        input_1: Parameter,
        input_2: Parameter,
        output_addr: Parameter,
    },
    Halt,
}

impl IntCode {
    pub fn run(&mut self) -> Result<()> {
        let mut pc: usize = 0;
        self.running = true;
        while self.running {
            let instr = self.read_instr_at(&mut pc)?;
            self.execute_instr(instr, &mut pc);
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

    pub fn queue_input(&mut self, value: TInt) {
        self.input.push(value)
    }

    pub fn read_output(&mut self) -> Option<TInt> {
        self.output.pop()
    }

    pub fn print_memory(&self) {
        for chunk in self.memory.chunks(10) {
            for cell in chunk {
                print!("{cell}\t")
            }
            println!()
        }
    }

    fn split_opcode(opcode: TInt) -> (u8, u8, u8, u8) {
        let mut opcode = opcode;
        let opnum = (opcode % 100) as u8;
        opcode /= 100;
        let param1_mode = (opcode % 10) as u8;
        opcode /= 10;
        let param2_mode = (opcode % 10) as u8;
        opcode /= 10;
        let param3_mode = (opcode % 10) as u8;

        (opnum, param1_mode, param2_mode, param3_mode)
    }

    fn read_instr_at(&self, position: &mut usize) -> Result<Instruction> {
        let (opnum, param1_mode, param2_mode, param3_mode) =
            Self::split_opcode(self.memory[*position]);

        let param1 = || Parameter::make(param1_mode, self.memory[*position + 1]);
        let param2 = || Parameter::make(param2_mode, self.memory[*position + 2]);
        let param3 = || Parameter::make(param3_mode, self.memory[*position + 3]);

        match opnum {
            1 => {
                let instr = Instruction::Add {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                *position += 4;
                Ok(instr)
            }
            2 => {
                let instr = Instruction::Mul {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                *position += 4;
                Ok(instr)
            }
            3 => {
                let instr = Instruction::Input {
                    output_addr: param1()?,
                };
                *position += 2;
                Ok(instr)
            }
            4 => {
                let instr = Instruction::Output { input: param1()? };
                *position += 2;
                Ok(instr)
            }
            5 => {
                let instr = Instruction::JumpNZ {
                    input: param1()?,
                    target: param2()?,
                };
                *position += 3;
                Ok(instr)
            }
            6 => {
                let instr = Instruction::JumpZ {
                    input: param1()?,
                    target: param2()?,
                };
                *position += 3;
                Ok(instr)
            }
            7 => {
                let instr = Instruction::LT {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                *position += 4;
                Ok(instr)
            }
            8 => {
                let instr = Instruction::Eq {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                *position += 4;
                Ok(instr)
            }
            99 => {
                let instr = Instruction::Halt;
                *position += 1;
                Ok(instr)
            }
            other => Err(format!("unknown opcode {other}").into()),
        }
    }

    fn execute_instr(&mut self, instr: Instruction, position: &mut usize) {
        match instr {
            Instruction::Add {
                input_1,
                input_2,
                output_addr,
            } => {
                let a = input_1.get_value(self);
                let b = input_2.get_value(self);
                let output_addr = output_addr.as_output_address(self);
                self.memory[output_addr] = a + b;
            }
            Instruction::Mul {
                input_1,
                input_2,
                output_addr,
            } => {
                let a = input_1.get_value(self);
                let b = input_2.get_value(self);
                let output_addr = output_addr.as_output_address(self);
                self.memory[output_addr] = a * b;
            }
            Instruction::Input { output_addr } => {
                let a = self.input.pop().expect("expecting a value to input");
                let output_addr = output_addr.as_output_address(self);
                self.memory[output_addr] = a;
            }
            Instruction::Output { input } => {
                let a = input.get_value(self);
                self.output.push(a);
            }
            Instruction::JumpNZ { input, target } => {
                if input.get_value(self) != 0 {
                    *position = target.as_address(self);
                }
            }
            Instruction::JumpZ { input, target } => {
                if input.get_value(self) == 0 {
                    *position = target.as_address(self);
                }
            }
            Instruction::LT {
                input_1,
                input_2,
                output_addr,
            } => {
                let output_addr = output_addr.as_output_address(self);
                self.memory[output_addr] =
                    i32::from(input_1.get_value(self) < input_2.get_value(self));
            }
            Instruction::Eq {
                input_1,
                input_2,
                output_addr,
            } => {
                let output_addr = output_addr.as_output_address(self);
                self.memory[output_addr] =
                    i32::from(input_1.get_value(self) == input_2.get_value(self));
            }
            Instruction::Halt => self.running = false,
        }
    }
}

impl FromStr for IntCode {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self> {
        let nums = s
            .trim()
            .split(',')
            .map(|x| {
                TInt::from_str_radix(x, 10)
                    .map_err(|e| format!("failed to parse {x:?} as a number: {e}"))
            })
            .collect::<std::result::Result<Vec<_>, _>>()?;
        Ok(IntCode {
            memory: nums,
            input: vec![],
            output: vec![],
            running: false,
        })
    }
}
