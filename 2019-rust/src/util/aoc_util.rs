use color_eyre::eyre::{eyre, Context, Report, Result};
use std::{
    fs,
    path::{Path, PathBuf},
};

use reqwest::{blocking::Client, cookie::Jar, Url};

pub fn get_input(year: u16, day: u8) -> Result<String> {
    let cookie = fs::read_to_string("./.cookie").wrap_err(
        "Need to be able to read .cookie file to get AoC input: \
make sure this file exists and you are in the correct directory",
    )?;

    let input_file_path = PathBuf::from(format!("input/day_{day}.txt"));
    if Path::new(&input_file_path).exists() {
        return Ok(fs::read_to_string(input_file_path)?);
    }

    let cookie_store = Jar::default();
    let cookie_string = format!("session={cookie}");
    let url = "https://adventofcode.com".parse::<Url>().unwrap();
    cookie_store.add_cookie_str(&cookie_string, &url);
    let client = Client::builder()
        .cookie_provider(cookie_store.into())
        .user_agent("github.com/nyctef/advent-of-code")
        .build()?;
    let puzzle_string = client
        .get(format!("https://adventofcode.com/{year}/day/{day}/input"))
        .send()
        .map_err(|e| Report::new(e))
        .and_then(|r| {
            if r.status().is_client_error() || r.status().is_server_error() {
                Err(eyre!("{}: {}", r.status().as_u16(), r.text()?))
            } else {
                Ok(r.text()?)
            }
        })?;

    fs::create_dir_all(input_file_path.parent().unwrap())
        .wrap_err("failed to create folder for input files")?;
    fs::write(input_file_path, &puzzle_string).wrap_err("failed to write puzzle input file")?;

    Ok(puzzle_string)
}
