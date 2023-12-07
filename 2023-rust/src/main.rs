mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod utils;

use color_eyre::{eyre::Result, Report};

fn main() -> Result<()> {
    color_eyre::install()?;

    let usage = "Usage: [exe] [day]";
    let day: u8 = std::env::args()
        .nth(1)
        .ok_or_else(|| Report::msg(usage))?
        .parse()
        .map_err(|_| Report::msg(usage.to_owned() + ": failed to parse day as u8"))?;

    match day {
        1 => day01::solve(),
        2 => day02::solve(),
        3 => day03::solve(),
        4 => day04::solve(),
        5 => day05::solve(),
        6 => day06::solve(),
        7 => day07::solve(),
        other => Err(Report::msg(format!("day {other} not implemented yet"))),
    }
}
