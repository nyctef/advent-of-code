mod aoc_util;
mod day01;

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
        other => return Err(Report::msg(format!("day {other} not implemented yet"))),
    }
}
