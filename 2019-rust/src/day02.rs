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

impl<T: num_traits::Num> FromStr for IntCode<T> {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self> {
        let nums = s
            .trim()
            .split(",")
            .map(|x| T::from_str_radix(x, 10))
            .into_iter()
            .collect::<std::result::Result<Vec<_>, T::FromStrRadixErr>>()
            .map_err(|_e| "failed to parse input as a number")?;
        Ok(IntCode { data: nums })
    }
}

pub fn solve() -> Result<()> {
    let input = get_input(2019, 2)?;

    let intcode: IntCode<u32> = input.parse()?;
    dbg!(intcode.data);

    Ok(())
}

#[test]
fn test1() {
    // ...
}
