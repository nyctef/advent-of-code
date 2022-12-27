mod aoc_util;
mod day01;
mod day02;
mod err_util;

use err_util::*;

fn main() -> Result<()> {
    let usage = "Usage: [exe] [day]";
    let day: u8 = std::env::args()
        .nth(1)
        .ok_or(usage)?
        .parse()
        .map_err(|_| usage.to_owned() + ": failed to parse day as u8")?;

    match day {
        1 => day01::solve(),
        2 => day02::solve(),
        other => return Err(format!("day {other} not implemented yet").into()),
    }
}
