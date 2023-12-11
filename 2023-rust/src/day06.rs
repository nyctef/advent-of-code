use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 6)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let races = input
        .trim()
        .replace(" ", "")
        .lines()
        .map(|l| l.split_once(':').unwrap().1)
        .map(all_numbers_u64)
        .collect_vec();
    let races = races[0].iter().zip(races[1].iter()).collect_vec();

    dbg!(&races);

    let mut part1 = 1;
    for (&time, &distance) in races {
        let mut num_ways = 0;
        for way in 0..=time {
            if way * (time - way) > distance {
                num_ways += 1;
            }
        }
        part1 *= num_ways;
    }

    let part2 = "";

    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
#[ignore]
fn test_example1() -> Result<()> {
    let input = r###"
Time:      7  15   30
Distance:  9  40  200
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 288 | Part 2: ", result);
    Ok(())
}
