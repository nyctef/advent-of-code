use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 5)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let mut jumps = input
        .trim()
        .lines()
        .map(|l| l.parse::<isize>().unwrap())
        .collect_vec();

    let mut part1 = 0;
    let mut part2 = 0;

    let mut pc: isize = 0;
    loop {
        if pc < 0 || pc as usize >= jumps.len() {
            break;
        }
        part1 += 1;
        let jump = jumps[pc as usize];
        jumps[pc as usize] += 1;
        pc += jump;
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
0
3
0
1
-3
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 5);
    assert_eq!(part2, 0);
}
