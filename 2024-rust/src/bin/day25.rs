use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 25)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let items = input.trim().split("\n\n").map(|i| i.trim()).collect_vec();
    let locks = items
        .iter()
        .filter(|i| i.starts_with("#####"))
        .map(|i| i[5..i.len() - 5].trim())
        .collect_vec();
    let keys = items
        .iter()
        .filter(|i| i.ends_with("#####"))
        .map(|i| i[5..i.len() - 5].trim())
        .collect_vec();

    let locks = locks
        .into_iter()
        .map(|l| {
            let res = get_heights(l);

            res
        })
        .collect_vec();
    let keys = keys
        .into_iter()
        .map(|l| {
            let res = get_heights(l);

            res
        })
        .collect_vec();

    let mut part1 = 0;
    for lock in locks {
        'next_key: for key in &keys {
            for i in 0..5 {
                if lock[i] + key[i] >5 {
                    continue 'next_key;
                }

            }
            part1 += 1;

        }
    }

    let mut part2 = 0;

    (part1, part2)
}

fn get_heights(l: &str) -> Vec<i32> {
    let lines = l.lines().collect_vec();
    let mut res = vec![];
    for i in 0..5 {
        let mut height = 0;
        for line in &lines {
            if line.chars().nth(i).unwrap() == '#' {
                height += 1;
            }
        }

        res.push(height);
    }
    res
}

#[test]
fn test_example1() {
    let input = r###"
    
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 0);
    assert_eq!(part2, 0);
}
