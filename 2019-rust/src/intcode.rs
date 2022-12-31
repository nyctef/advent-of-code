use color_eyre::{eyre::Result, Report};
use std::{collections::VecDeque, str::FromStr};

// TODO: can we actually make IntCode generic on the size of the integer?
pub type TInt = i64;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MachineState {
    Halted,
    Running,
    AwaitingInput,
}

#[derive(Debug, Clone)]
pub struct IntCode {
    memory: Vec<TInt>,
    input: VecDeque<TInt>,
    output: VecDeque<TInt>,
    pc: usize,
    relative_base: usize,
    state: MachineState,
}

#[derive(Debug)]
enum Parameter {
    Address(usize),
    Value(TInt),
    Relative(TInt),
}

fn update_relative_base(rb: usize, diff: TInt) -> usize {
    let rb: TInt = rb.try_into().unwrap();
    let result: TInt = rb + diff;
    result.try_into().unwrap()
}

impl Parameter {
    fn get_value(&self, ic: &IntCode) -> TInt {
        match self {
            Parameter::Address(addr) => ic.memory[*addr],
            Parameter::Value(value) => *value,
            Parameter::Relative(value) => ic.memory[update_relative_base(ic.relative_base, *value)],
        }
    }
    fn as_output_address(&self, ic: &IntCode) -> usize {
        match self {
            Parameter::Address(x) => *x,
            Parameter::Value(_) => panic!(),
            Parameter::Relative(x) => update_relative_base(ic.relative_base, *x),
        }
    }

    fn as_address(&self, ic: &IntCode) -> usize {
        self.get_value(ic).try_into().unwrap()
    }

    fn make(param_mode: u8, value: TInt) -> Result<Parameter> {
        let param = match param_mode {
            0 => Parameter::Address(value.try_into().map_err(|e| {
                Report::msg(format!(
                    "Failed to convert value {value:?} to a usize to use as a memory address: {e}"
                ))
            })?),
            1 => Parameter::Value(value),
            2 => Parameter::Relative(value),
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
    Rebase {
        adjustment: Parameter,
    },
    Halt,
}

impl IntCode {
    pub fn run(&mut self) -> Result<()> {
        self.state = MachineState::Running;
        while self.state == MachineState::Running {
            let instr = self.read_instr_at_pc()?;
            self.execute_instr(instr);
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
        self.input.push_front(value)
    }

    pub fn read_output(&mut self) -> Option<TInt> {
        self.output.pop_front()
    }

    pub fn state(&self) -> (usize, MachineState) {
        (self.pc, self.state)
    }

    #[cfg(test)]
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

    fn read_instr_at_pc(&self) -> Result<Instruction> {
        let (opnum, param1_mode, param2_mode, param3_mode) =
            Self::split_opcode(self.memory[self.pc]);

        let param1 = || Parameter::make(param1_mode, self.memory[self.pc + 1]);
        let param2 = || Parameter::make(param2_mode, self.memory[self.pc + 2]);
        let param3 = || Parameter::make(param3_mode, self.memory[self.pc + 3]);

        match opnum {
            1 => {
                let instr = Instruction::Add {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                Ok(instr)
            }
            2 => {
                let instr = Instruction::Mul {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                Ok(instr)
            }
            3 => {
                let instr = Instruction::Input {
                    output_addr: param1()?,
                };
                Ok(instr)
            }
            4 => {
                let instr = Instruction::Output { input: param1()? };
                Ok(instr)
            }
            5 => {
                let instr = Instruction::JumpNZ {
                    input: param1()?,
                    target: param2()?,
                };
                Ok(instr)
            }
            6 => {
                let instr = Instruction::JumpZ {
                    input: param1()?,
                    target: param2()?,
                };
                Ok(instr)
            }
            7 => {
                let instr = Instruction::LT {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                Ok(instr)
            }
            8 => {
                let instr = Instruction::Eq {
                    input_1: param1()?,
                    input_2: param2()?,
                    output_addr: param3()?,
                };
                Ok(instr)
            }
            9 => {
                let instr = Instruction::Rebase {
                    adjustment: param1()?,
                };
                Ok(instr)
            }
            99 => {
                let instr = Instruction::Halt;
                Ok(instr)
            }
            other => Err(Report::msg(format!("unknown opcode {other}"))),
        }
    }

    fn execute_instr(&mut self, instr: Instruction) {
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
                self.pc += 4;
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
                self.pc += 4;
            }
            Instruction::Input { output_addr } => {
                let a = self.input.pop_back();
                match a {
                    Some(value) => {
                        let output_addr = output_addr.as_output_address(self);
                        self.memory[output_addr] = value;
                        self.pc += 2;
                    }
                    None => {
                        // stop running until input is provided.
                        self.state = MachineState::AwaitingInput;
                        // note that we don't touch the program counter here - since
                        // self.pc should have the same value the next time the machine
                        // starts up, the program should continue as normal once input
                        // is provided
                    }
                }
            }
            Instruction::Output { input } => {
                let a = input.get_value(self);
                self.output.push_back(a);
                self.pc += 2;
            }
            Instruction::JumpNZ { input, target } => {
                if input.get_value(self) != 0 {
                    self.pc = target.as_address(self);
                } else {
                    self.pc += 3;
                }
            }
            Instruction::JumpZ { input, target } => {
                if input.get_value(self) == 0 {
                    self.pc = target.as_address(self);
                } else {
                    self.pc += 3;
                }
            }
            Instruction::LT {
                input_1,
                input_2,
                output_addr,
            } => {
                let output_addr = output_addr.as_output_address(self);
                self.memory[output_addr] =
                    TInt::from(input_1.get_value(self) < input_2.get_value(self));
                self.pc += 4;
            }
            Instruction::Eq {
                input_1,
                input_2,
                output_addr,
            } => {
                let output_addr = output_addr.as_output_address(self);
                self.memory[output_addr] =
                    TInt::from(input_1.get_value(self) == input_2.get_value(self));
                self.pc += 4;
            }
            Instruction::Rebase { adjustment } => {
                let adjustment = adjustment.get_value(self);
                self.relative_base = update_relative_base(self.relative_base, adjustment);
                self.pc += 2;
            }
            Instruction::Halt => self.state = MachineState::Halted,
        }
    }
}

impl FromStr for IntCode {
    type Err = Report;

    fn from_str(s: &str) -> Result<Self> {
        let mut nums = s
            .trim()
            .split(',')
            .map(|x| {
                TInt::from_str_radix(x, 10)
                    .map_err(|e| format!("failed to parse {x:?} as a number: {e}"))
                    .map_err(Report::msg)
            })
            .collect::<std::result::Result<Vec<_>, _>>()?;

        nums.resize(nums.len() + 1000, 0);

        Ok(IntCode {
            memory: nums,
            input: vec![].into(),
            output: vec![].into(),
            pc: 0,
            relative_base: 0,
            state: MachineState::Halted,
        })
    }
}
