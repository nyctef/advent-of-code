use crate::aoc_util::*;
use color_eyre::eyre::Result;
use itertools::{chain, repeat_n, Itertools};

pub fn solve() -> Result<()> {
    let input = get_input(2019, 1)?;

    let result = solve_for(&input, 100)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, num_phases: u8) -> Result<String> {
    let parsed = input.chars().map(|c| c.to_digit(10)).collect_vec();

    todo!()
}

fn make_pattern(repeats: usize) -> impl Iterator<Item = i8> {
    // eg if repeats is 2, then this produces an infinite cycle of
    // 0, 0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1, ...
    chain!(
        repeat_n(0, repeats),
        repeat_n(1, repeats),
        repeat_n(0, repeats),
        repeat_n(-1, repeats)
    )
    .cycle()
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"12345678"###;
    let result = solve_for(input, 4)?;

    assert_eq!("01029498", result);
    Ok(())
}
