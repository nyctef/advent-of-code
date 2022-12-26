use std::{error::Error, fs, path::Path};

use reqwest::{blocking::Client, cookie::Jar, Url};

pub fn get_input(year: u16, day: u8) -> Result<String, Box<dyn Error>> {
    let cookie = fs::read_to_string("./.cookie").expect(
        "Need to be able to read .cookie file to get AoC input: \
make sure this file exists and you are in the correct directory",
    );

    let input_file_path = format!("input/day_{day}.txt");
    if Path::new(&input_file_path).exists() {
        return Ok(fs::read_to_string(input_file_path)?);
    }

    let cookie_store = Jar::default();
    let cookie_string = format!("session={cookie}");
    let url = "https://adventofcode.com".parse::<Url>().unwrap();
    cookie_store.add_cookie_str(&cookie_string, &url);
    let client = Client::builder()
        .cookie_provider(cookie_store.into())
        .build()?;
    let puzzle_string = client
        .get(format!("https://adventofcode.com/{year}/day/{day}/input"))
        .send()?
        .text()?;

    fs::write(input_file_path, &puzzle_string)?;

    Ok(puzzle_string)
}
