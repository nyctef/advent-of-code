use chrono::{Datelike, Local, DateTime};
use color_eyre::{eyre::{eyre, Report}, Result};

fn main() -> Result<()> {
    color_eyre::install()?;

    let usage = r"Usage: cargo xtask <task> [day]";

    let mut args = std::env::args();
    let task = args.next().ok_or_else(|| eyre!(usage));
    let day = args.next().map(|d| d.parse::<u8>().map_err(Report::new));

    let now = Local::now();
    let day = resolve_day(day, now);

    Ok(())
}

fn resolve_day(
    specific_day: Option<Result<u8>>,
    now: DateTime<Local>,
) -> Result<u8> {

    if let Some(Ok(specific_day)) = specific_day {
        return Ok(specific_day);
    } else if let Some(Err(day_error)) = specific_day {
        return Err(day_error);
    }

    // otherwise try to infer the aoc day from the current date
    if now.month() == 12 && now.day() <= 25 {
        return Ok(now.day() as u8);
    }

    return Err(eyre!("A day needs to be specified, since it isn't advent"));

}
