use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use regex::Regex;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 3)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let input = input.trim();

    let mut part1: u64 = 0;
    let part1_re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    for (_, [x, y]) in part1_re.captures_iter(input).map(|c| c.extract()) {
        part1 += x.parse::<u64>().unwrap() * y.parse::<u64>().unwrap();
    }

    let part2_re = Regex::new(r"do\(\)|don't\(\)|mul\((\d+),(\d+)\)").unwrap();
    let mut part2: u64 = 0;
    let mut enabled = true;
    for cap in part2_re.captures_iter(input) {
        //dbg!(&cap, &cap.get(0).unwrap().as_str());
        if cap.get(0).unwrap().as_str() == "do()" {
            enabled = true;
        } else if cap.get(0).unwrap().as_str() == "don't()" {
            enabled = false;
        } else if enabled {
            let x = cap.get(1).unwrap().as_str();
            let y = cap.get(2).unwrap().as_str();
            part2 += x.parse::<u64>().unwrap() * y.parse::<u64>().unwrap();
        }
    }
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 161);
    assert_eq!(part2, 161);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 161);
    assert_eq!(part2, 48);
    Ok(())
}
