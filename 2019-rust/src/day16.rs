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
    let parsed = input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as i32)
        .collect_vec();

    let mut signal = parsed;
    for phase in 0..num_phases {
        // println!("phase {phase}: before: {:?}", &signal);
        signal = (0..signal.len())
            .map(|i| {
                signal
                    .iter()
                    .zip(make_pattern(i + 1))
                    .map(|(&x, p)| (x * p as i32))
                    .sum::<i32>()
                    .abs()
                    % 10
            })
            .collect_vec();
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
fn test_example1() -> Result<()> {
    let input = r###"
    12345678
    "###;
    let result = solve_for(input, 4)?;

    assert_eq!("01029498", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
    80871224585914546619083218645595
    "###;
    let result = solve_for(input, 100)?;

    assert_eq!("24176176", result);
    Ok(())
}
#[test]
fn test_pattern() {
    let result = make_pattern(2).take(5).collect_vec();
    assert_eq!(vec![0, 1, 1, 0, 0], result);

    assert_eq!(vec![1, 0, -1, 0, 1], make_pattern(1).take(5).collect_vec());
}
