use std::fmt::Display;
use std::str::FromStr;

use crate::aoc_util::*;
use crate::err_util::*;

#[derive(Debug)]
struct IntCode<T>
where
    T: num_traits::Num,
{
    data: Vec<T>,
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

impl<T: num_traits::Num + Display> IntCode<T>
where
    usize: TryFrom<T>,
    u8: TryFrom<T>,
{
    /// Overwrites the value at position (zero-indexed)
    fn set_value_at_position(&mut self, position: usize, value: T) {
        self.data[position] = value;
    }

    fn read_op_at(&self, position: usize) -> Result<Opcode> {
        let opnum: u8 = self.data[position].try_into()?;
        let opcode = match opnum {
            1 => Opcode::Add {
                input_pos_1: self.data[position + 1].try_into()?,
                input_pos_2: self.data[position + 2].try_into()?,
                output_pos: self.data[position + 3].try_into()?,
            },
            other => todo!("unknown opcode {other}"),
        };
        Ok(opcode)
    }
}

impl<T: num_traits::Num> FromStr for IntCode<T> {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self> {
        let nums = s
            .trim()
            .split(",")
            .map(|x| T::from_str_radix(x, 10))
            .collect::<std::result::Result<Vec<_>, T::FromStrRadixErr>>()
            .map_err(|_e| "failed to parse input as a number")?;
        Ok(IntCode { data: nums })
    }
}

pub fn solve() -> Result<()> {
    let input = get_input(2019, 2)?;

    let mut intcode: IntCode<u32> = input.parse()?;
    intcode.set_value_at_position(1, 12);
    intcode.set_value_at_position(2, 2);

    let op1 = intcode.read_op_at(0);
    dbg!(op1);

    Ok(())
}

#[test]
fn test1() {
    // ...
}
