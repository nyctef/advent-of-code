use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 13)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (i64, i64) {
    let mut part1 = 0;
    let mut part2 = 0;

    let mut scanners = input
        .trim()
        .lines()
        .map(|l| {
            let nums = all_numbers_i64(l);
            // depth, length
            (nums[0], nums[1])
        })
        .collect_vec();

    for &(depth, length) in &scanners {
        let cycle = (length - 1) * 2;
        if depth % cycle == 0 {
            part1 += length * depth;
        }
    }

    'next_delay: for delay in 1.. {
        for &(depth, length) in &scanners {
            let cycle = (length - 1) * 2;
            let time = depth + delay;
            if time % cycle == 0 {
                // caught!
                continue 'next_delay;
            }
        }
        part2 = delay;
        break;
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
0: 3
1: 2
4: 4
6: 4
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 24);
    assert_eq!(part2, 10);
}
