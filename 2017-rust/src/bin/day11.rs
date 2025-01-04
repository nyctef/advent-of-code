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
    let mut nw_se: isize = 0;

    for step in input.trim().split(",") {
        match step {
            "n" => n_s += 1,
            "s" => n_s -= 1,
            "ne" => ne_sw += 1,
            "sw" => ne_sw -= 1,
            "nw" => nw_se += 1,
            "se" => nw_se -= 1,
            _ => panic!("unrecognised direction {step}"),
        }
    }

    if nw_se > 0 && ne_sw > 0 {
        let common = nw_se.min(ne_sw);
        nw_se -= common;
        ne_sw -= common;
        n_s += common;
    }

    if nw_se < 0 && ne_sw < 0 {
        let common = nw_se.max(ne_sw);
        nw_se -= common;
        ne_sw -= common;
        n_s += common;
    }



    dbg!(&n_s, ne_sw, nw_se);
    part1 = n_s.abs() + ne_sw.abs() + nw_se.abs();

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
