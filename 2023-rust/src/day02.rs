use crate::aoc_util::*;
use color_eyre::eyre::{eyre, Result};
use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{newline, u32},
    combinator::{cut, eof, map, map_res},
    error::{convert_error, ContextError, ParseError, VerboseError},
    multi::separated_list0,
    sequence::{pair, preceded, terminated},
    Finish, IResult,
};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 2)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(Debug)]
struct Game {
    #[allow(dead_code)]
    num: u32,
    picks: Vec<Pick>,
}

#[derive(Debug)]
struct Pick {
    green: u32,
    red: u32,
    blue: u32,
}

#[derive(PartialEq, Eq)]
enum Color {
    Red,
    Green,
    Blue,
}

fn count_color(input: &[(u32, Color)], color: Color) -> u32 {
    input.iter().filter(|x| x.1 == color).map(|x| x.0).sum()
}

fn games<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + nom::error::FromExternalError<&'a str, nom::Err<(&'a str, nom::error::ErrorKind)>>,
>(
    input: &'a str,
) -> IResult<&'a str, Vec<Game>, E> {
    let game_num = preceded(tag("Game "), u32);
    let color = map_res(
        alt((tag("red"), tag("green"), tag("blue"))),
        |s: &str| match s {
            "red" => Ok(Color::Red),
            "green" => Ok(Color::Green),
            "blue" => Ok(Color::Blue),
            _ => Err(nom::Err::Failure((s, nom::error::ErrorKind::Tag))),
        },
    );
    let color_pick = pair(terminated(u32, tag(" ")), color);
    let pick = map(separated_list0(tag(", "), color_pick), |color_picks| Pick {
        red: count_color(&color_picks, Color::Red),
        green: count_color(&color_picks, Color::Green),
        blue: count_color(&color_picks, Color::Blue),
    });
    let picks = map(separated_list0(tag("; "), pick), |picks| {
        picks.into_iter().collect()
    });
    let game = map(
        pair(terminated(game_num, tag(": ")), picks),
        |(num, picks)| Game { num, picks },
    );
    let mut games = terminated(separated_list0(newline, cut(game)), eof);
    games(input)
}

fn parse(input: &str) -> Result<Vec<Game>> {
    let parse_result = games::<VerboseError<&str>>(input)
        .finish()
        .map_err(|e| eyre!(convert_error(input, e)))?;
    Ok(parse_result.1)
}

fn solve_for(input: &str) -> Result<String> {
    let parsed_games = parse(input.trim())?;

    let minimum_picks = parsed_games
        .iter()
        .map(|g| Pick {
            red: g.picks.iter().map(|p| p.red).max().unwrap_or(0),
            blue: g.picks.iter().map(|p| p.blue).max().unwrap_or(0),
            green: g.picks.iter().map(|p| p.green).max().unwrap_or(0),
        })
        .collect_vec();

    let result: u32 = minimum_picks.iter().map(|p| p.red * p.blue * p.green).sum();

    Ok(result.to_string())
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"###;
    let result = solve_for(input)?;

    assert_eq!("2286", result);
    Ok(())
}
