use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

use chrono::{DateTime, Datelike, Local};
use color_eyre::{
    eyre::{eyre, Context, Report},
    Result,
};

fn main() -> Result<()> {
    color_eyre::install()?;

    let usage = r"Usage: cargo xtask <task> [day]";

    let mut args = std::env::args();
    let _exe = args.next();
    let task = args.next().ok_or_else(|| eyre!(usage))?;
    let day = args.next().map(|d| d.parse::<u8>().map_err(Report::new));

    let now = Local::now();
    let day = resolve_day(day, now)?;

    match &task[..] {
        "new_day" => new_day(day),
        "test_day" => test_day(day),
        "run_day" => run_day(day),
        _ => Err(eyre!("Unknown task: {task}")),
    }
}

fn new_day(day: u8) -> Result<()> {
    let root = root_dir();
    let source = root.join("src/util/_template.rs");
    let target = root.join(format!("src/bin/day{day:0>2}.rs"));

    let mut new_file_contents = fs::read_to_string(source).wrap_err("reading template file")?;
    new_file_contents = new_file_contents.replace("__DAY__", &day.to_string());
    fs::write(target, new_file_contents).wrap_err("writing new day file")?;

    Ok(())
}

fn test_day(day: u8) -> Result<()> {
    Command::new("cargo")
        .current_dir(root_dir())
        .args(["test", "--bin", &format!("day{day:0>2}")])
        .spawn()?
        .wait()?;
    Ok(())
}

fn run_day(day: u8) -> Result<()> {
    Command::new("cargo")
        .current_dir(root_dir())
        .args(["run", "--release", "--bin", &format!("day{day:0>2}")])
        .spawn()?
        .wait()?;
    Ok(())
}

fn root_dir() -> PathBuf {
    Path::new(
        // use runtime environment variable, or fall back to env!(...)
        // which gives us the compile-time value of that variable.
        // They should be equivalent (especially when building locally)
        // but we're just copying this pattern from rust-analyzer
        // xtasks for now without thinking too hard.
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .to_path_buf()
}

fn resolve_day(specific_day: Option<Result<u8>>, now: DateTime<Local>) -> Result<u8> {
    if let Some(Ok(specific_day)) = specific_day {
        return Ok(specific_day);
    } else if let Some(Err(day_error)) = specific_day {
        return Err(day_error).wrap_err("attempting to parse day value");
    }

    // otherwise try to infer the aoc day from the current date
    if now.month() == 12 && now.day() <= 25 {
        return Ok(now.day() as u8);
    }

    return Err(eyre!("A day needs to be specified, since it isn't advent"));
}
