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
mod day13;
mod day14;
mod intcode;
use color_eyre::{eyre::Result, Report};

fn main() -> Result<()> {
    color_eyre::install()?;

    let usage = "Usage: [exe] [day]";
    let day: u8 = std::env::args()
        .nth(1)
        .ok_or(Report::msg(usage))?
        .parse()
        .map_err(|_| Report::msg(usage.to_owned() + ": failed to parse day as u8"))?;

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
        13 => day13::solve(),
        14 => day14::solve(),
        other => return Err(Report::msg(format!("day {other} not implemented yet"))),
    }
}
