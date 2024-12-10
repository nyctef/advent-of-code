use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 20)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let target: u64 = input.trim().parse().unwrap();

    let mut i = 1;
    loop {
        let mut total = 0;
        for elf in 1..=i {
            if i % elf == 0 {
                total += elf * 10;
            }
        }

        if i < 10 || i % 10_000 == 0 {
            eprintln!("House {i} got {total} presents");
        }

        if total >= target {
            eprintln!("House {i} got {total} presents");
            break;
        }

        i += 1;
    }

    todo!();
    let part1 = "";
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: ", result);
    Ok(())
}
