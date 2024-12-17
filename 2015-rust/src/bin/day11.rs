use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::str::from_utf8;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 11)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let input = &input.trim().to_string();
    let mut password = input.clone().into_bytes();
    while !is_valid(&password) {
        incr(&mut password);
    }
    let part1 = from_utf8(&password).unwrap();

    let mut password = password.clone();
    incr(&mut password);
    while !is_valid(&password) {
        incr(&mut password);
    }
    let part2 = from_utf8(&password).unwrap();

    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn incr(value: &mut [u8]) {
    let mut pos = value.len() - 1;
    loop {
        let old_char = value[pos];
        if old_char >= b'z' {
            value[pos] = b'a';
            pos -= 1;
            continue;
        } else {
            value[pos] = old_char + 1_u8;
            break;
        }
    }
}

fn is_valid(value: &[u8]) -> bool {
    let has_straight = value
        .iter()
        .tuple_windows()
        .any(|(&a, &b, &c)| a + 1 == b && b + 1 == c);
    let has_confusing_characters = value.iter().any(|c| matches!(c, b'i' | b'o' | b'l'));
    let has_repeated_pair = value
        .iter()
        // list all pairs of chars
        .tuple_windows::<(_, _)>()
        // attach a position to each pair
        .enumerate()
        // check that the pairs are pairs
        .filter(|&(_p, (a, b))| a == b)
        // forget what pair it was - just remember its position
        .map(|t| t.0)
        // check to see if the pairs are far enough apart
        // ie we don't want this situation:
        //    aaa
        //    ^^
        //    01
        // to count if we detect pairs starting at indexes 0 and 1
        .tuple_windows()
        .any(|(a, b)| b - a > 1);

    // println!(
    //     "v: {:?} has_straight: {} has_repeated_pair: {} has_confusing: {}",
    //     value, has_straight, has_repeated_pair, has_confusing_characters
    // );
    has_straight && has_repeated_pair && !has_confusing_characters
}

#[test]
fn test_incr() {
    let mut value = "xy".to_string().into_bytes();
    incr(&mut value);
    assert_eq!(from_utf8(&value).unwrap(), "xz");
    // incrementing the z in the last position triggers a carry
    incr(&mut value);
    assert_eq!(from_utf8(&value).unwrap(), "ya");
    incr(&mut value);
    assert_eq!(from_utf8(&value).unwrap(), "yb");
}

#[test]
fn test_is_valid() {
    assert!(!is_valid(&"hijklmmn".to_string().into_bytes()));
    assert!(!is_valid(&"abbceffg".to_string().into_bytes()));
    assert!(!is_valid(&"abbcegjk".to_string().into_bytes()));
    assert!(is_valid(&"abcdffaa".to_string().into_bytes()));
    assert!(is_valid(&"ghjaabcc".to_string().into_bytes()));
    assert!(!is_valid(&"ghjabccc".to_string().into_bytes()));
}
