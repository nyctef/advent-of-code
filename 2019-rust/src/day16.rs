use std::{
    io::{self, Write},
    vec,
};

use crate::aoc_util::*;
use color_eyre::eyre::Result;
use itertools::{chain, repeat_n, Itertools};

pub fn solve() -> Result<()> {
    let input = get_input(2019, 16)?;

    let result = solve_for(&input, 100)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, num_phases: u8) -> Result<String> {
    let offset = input
        .trim()
        .chars()
        .take(7)
        .join("")
        .parse::<usize>()
        .unwrap();

    let parsed = repeat_n(
        input.trim().chars().map(|c| c.to_digit(10).unwrap() as i32),
        10_000,
    )
    .flatten()
    .skip(offset)
    .collect_vec();

    println!(
        "offset is {}, revised signal length is {}",
        offset,
        parsed.len()
    );

    let mut signal = parsed;
    for _phase in 0..num_phases {
        print!(".");
        io::stdout().flush().unwrap();
        // println!("phase {phase}: before: {:?}", &signal);
        let mut next_signal = vec![];
        let mut acc = 0;
        for x in signal.iter().rev() {
            acc = (x + acc) % 10;
            next_signal.push(acc);
        }
        signal = next_signal.into_iter().rev().collect_vec();
        // signal = (0..signal.len())
        //     .map(|i| {
        //         signal
        //             .iter()
        //             .zip(make_pattern(i + 1))
        //             .map(|(&x, p)| (x * p as i32))
        //             .sum::<i32>()
        //             .abs()
        //             % 10
        //     })
        //     .collect_vec();
        // println!("after: {:?}", &signal);
    }

    Ok(signal.iter().take(8).join(""))
}

fn make_pattern(repeats: usize) -> impl Iterator<Item = i8> {
    // eg if repeats is 2, then this produces an infinite cycle of
    // 0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1, ...
    chain!(
        repeat_n(0, repeats),
        repeat_n(1, repeats),
        repeat_n(0, repeats),
        repeat_n(-1, repeats)
    )
    .cycle()
    .skip(1)
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
    03036732577212944063491565474664
    "###;
    let result = solve_for(input, 100)?;

    assert_eq!("84462026", result);
    Ok(())
}
#[test]
fn test_pattern() {
    let result = make_pattern(2).take(5).collect_vec();
    assert_eq!(vec![0, 1, 1, 0, 0], result);

    assert_eq!(vec![1, 0, -1, 0, 1], make_pattern(1).take(5).collect_vec());
}
