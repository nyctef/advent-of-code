mod aoc_util;
mod day01;
mod day02;
mod day03;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod err_util;
mod intcode;

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
        3 => day03::solve(),
        5 => day05::solve(),
        6 => day06::solve(),
        7 => day07::solve(),
        8 => day08::solve(),
        9 => day09::solve(),
        10 => day10::solve(),
        11 => day11::solve(),
        12 => day12::solve(),
        other => return Err(format!("day {other} not implemented yet").into()),
    }
}
