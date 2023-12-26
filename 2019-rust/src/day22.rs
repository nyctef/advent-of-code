use std::str::FromStr;

use crate::util::*;
use color_eyre::eyre::Result;
use num_traits::Euclid;
use regex::Regex;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 22)?;

    let result = solve_for(&input, 10_007, 2019)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, size: isize, target: isize) -> Result<String> {
    let mut pos = target;
    let re = Regex::from_str(r"-?\d+")?;

    for line in input.trim().lines() {
        
        println!("{}", line);
        if line.starts_with("deal into") {
            pos = size - pos - 1;
            continue;
        }
        let num:isize = re.find(line).unwrap().as_str().parse().unwrap();
        if line.starts_with("cut") {
            pos = (pos - num).rem_euclid(size);
        }
        
        if line.starts_with("deal with") {
            pos = (pos * num).rem_euclid(size);
        }
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
