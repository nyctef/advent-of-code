use color_eyre::{
    eyre::{eyre, Context, Report},
    Result,
};
use regex::Regex;
use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

fn main() -> Result<()> {
    color_eyre::install()?;

    let usage = r"Usage: cargo xtask <task> [day]";

    let mut args = std::env::args();
    let _exe = args.next();
    let task = args.next().ok_or_else(|| eyre!(usage))?;
    let day = args.next().map(|d| d.parse::<u8>().map_err(Report::new));

    let bin_files = get_bin_files()?;
    let day = resolve_day(day, bin_files)?;

    match &task[..] {
        // TODO: this +1 is required if we're inferring the day,
        // but it's a little confusing if the user actually specified one
        "new_day" => new_day(day + 1),
        "test_day" => test_day(day),
        "run_day" => run_day(day),
        _ => Err(eyre!("Unknown task: {task}")),
    }
}

fn get_bin_files() -> Result<Vec<String>> {
    let root = root_dir();
    let readdir = fs::read_dir(root.join("src/bin/"))?;

    let entries: Result<Vec<_>> = readdir.map(|x| x.map_err(Report::new)).collect();
    let file_names: Result<Vec<_>> = entries?
        .into_iter()
        .map(|e| {
            e.file_name()
                .into_string()
                .map_err(|e| eyre!("failed to convert osstring {:?} into string", e))
        })
        .collect();

    file_names
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

fn resolve_day(specific_day: Option<Result<u8>>, existing_bin_files: Vec<String>) -> Result<u8> {
    if let Some(Ok(specific_day)) = specific_day {
        return Ok(specific_day);
    } else if let Some(Err(day_error)) = specific_day {
        return Err(day_error).wrap_err("attempting to parse day value");
    }

    // otherwise try to infer the current aoc day based on the latest dayXX.rs file
    // that's in the repo

    let re = Regex::new(r"^day(\d\d).rs$")?;
    let latest_day = existing_bin_files
        .iter()
        .filter_map(|f| re.captures(f))
        .map(|c| c[1].parse::<u8>().unwrap())
        .max();

    latest_day.ok_or(eyre!(
        "Couldn't find a latest day in src/bin/dayXX.rs - day needs to be specified"
    ))
}
