use core::panic;

use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 11)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (isize, u64) {
    let mut part1 = 0;
    let mut part2 = 0;

    let mut n_s: isize = 0;
    let mut ne_sw: isize = 0;
    let mut se_nw: isize = 0;

    for step in input.trim().split(",") {
        match step {
            "n" => n_s += 1,
            "s" => n_s -= 1,
            "ne" => ne_sw += 1,
            "sw" => ne_sw -= 1,
            "se" => se_nw += 1,
            "nw" => se_nw -= 1,
            _ => panic!("unrecognised direction {step}"),
        }
    }

    dbg!(&n_s, ne_sw, se_nw);
    part1 = n_s.abs() + ne_sw.abs() + se_nw.abs();

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
se,sw,se,sw,sw
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 3);
    assert_eq!(part2, 0);
}
