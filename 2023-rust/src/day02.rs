use crate::aoc_util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::u32,
    combinator::{map, map_res},
    multi::separated_list0,
    sequence::{pair, preceded, terminated},
    IResult,
};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 2)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(Debug)]
struct Game {
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

fn game_num(input: &str) -> IResult<&str, u32> {
    preceded(tag("Game "), u32)(input)
}

fn color(input: &str) -> IResult<&str, Color> {
    map_res(
        alt((tag("red"), tag("green"), tag("blue"))),
        |s: &str| match s {
            "red" => Ok(Color::Red),
            "green" => Ok(Color::Green),
            "blue" => Ok(Color::Blue),
            _ => Err(nom::Err::Error((s, nom::error::ErrorKind::Tag))),
        },
    )(input)
}

fn color_pick(input: &str) -> IResult<&str, (u32, Color)> {
    pair(terminated(u32, tag(" ")), color)(input)
}

fn count_color(input: &[(u32, Color)], color: Color) -> u32 {
    input.iter().filter(|x| x.1 == color).map(|x| x.0).sum()
}

fn pick(input: &str) -> IResult<&str, Pick> {
    map(separated_list0(tag(", "), color_pick), |color_picks| Pick {
        red: count_color(&color_picks, Color::Red),
        green: count_color(&color_picks, Color::Green),
        blue: count_color(&color_picks, Color::Blue),
    })(input)
}

fn picks(input: &str) -> IResult<&str, Vec<Pick>> {
    map(separated_list0(tag("; "), pick), |picks| {
        picks.into_iter().collect()
    })(input)
}

fn game(input: &str) -> IResult<&str, Game> {
    map(
        pair(terminated(game_num, tag(": ")), picks),
        |(num, picks)| Game { num, picks },
    )(input)
}

fn solve_for(input: &str) -> Result<String> {
    let lines = input.lines().filter(|l| !l.is_empty()).collect_vec();

    // TODO: why doesn't this version work?
    // let parsed_games: Vec<_> = lines.iter().map(|l| game(l)).try_collect()?;
    let parsed_games = lines.iter().map(|l| game(l)).collect_vec();
    let parsed_games = parsed_games
        .iter()
        .filter_map(|g| g.as_ref().ok())
        .collect_vec();

    let minimum_picks = parsed_games
        .iter()
        .map(|g| Pick {
            red: g.1.picks.iter().map(|p| p.red).max().unwrap_or(0),
            blue: g.1.picks.iter().map(|p| p.blue).max().unwrap_or(0),
            green: g.1.picks.iter().map(|p| p.green).max().unwrap_or(0),
        })
        .collect_vec();

    dbg!(&minimum_picks);

    let result: u32 = minimum_picks.iter().map(|p| p.red * p.blue * p.green).sum();

    Ok(result.to_string())
    // Ok("asdf".to_string())
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
