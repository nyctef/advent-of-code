use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 2)?;

    let (part1, part2) = (part1(&input), part2(&input));

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn part1(input: &str) -> u64 {
    let sheet = input.trim().lines().map(all_numbers_u64).collect_vec();

    return sheet
        .iter()
        .map(|l| l.iter().max().unwrap() - l.iter().min().unwrap())
        .sum();
}
fn part2(input: &str) -> u64 {
    let sheet = input.trim().lines().map(all_numbers_u64).collect_vec();

    return sheet
        .iter()
        .map(|l| {
            for i1 in 0..l.len() {
                for i2 in 0..l.len() {
                    if i1 == i2 {
                        continue;
                    }
                    if l[i1] % l[i2] == 0 {
                        return l[i1] / l[i2];
                    }
                }
            }
            panic!()
        })
        .sum();
}
#[test]
fn test_example1() {
    let input = r###"
5 1 9 5
7 5 3
2 4 6 8
"###;
    let part1 = part1(input);

    assert_eq!(part1, 18);
}

#[test]
fn test_example2() {
    let input = r###"
5 9 2 8
9 4 7 3
3 8 6 5
"###;
    let part2 = part2(input);

    assert_eq!(part2, 9);
}
