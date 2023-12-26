use crate::util::*;
use color_eyre::eyre::Result;
use regex::Regex;
use std::str::FromStr;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 22)?;

    let result = solve_for(&input, 119315717514047, 2020, 101741582076661)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, size: isize, target: isize, iterations: usize) -> Result<String> {
    let mut pos = target;
    let re = Regex::from_str(r"-?\d+")?;

    let mut iteration = 0;
    let mut last_pos = 0;
    while iteration < iterations {
        for line in input.trim().lines() {
            // println!("{}", line);
            if line.starts_with("deal into") {
                pos = size - pos - 1;
                continue;
            }
            let num: isize = re.find(line).unwrap().as_str().parse().unwrap();
            if line.starts_with("cut") {
                pos = (pos - num).rem_euclid(size);
            }

            if line.starts_with("deal with") {
                pos = (pos * num).rem_euclid(size);
            }
            println!(
                "i {} pos {} pos-last_pos {} ",
                iteration,
                pos,
                pos - last_pos
            );
            last_pos = pos;
        }
        iteration += 1;
    }

    Ok(format!("final position of card {}: {}", target, pos))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
"###;
    let result = solve_for(input, 10, 7)?;

    assert_eq!("final position of card 7: 6", result);
    Ok(())
}
