use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::{izip, Itertools};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 15)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let mut part1 = 0;

    let nums = input
        .trim()
        .lines()
        .map(|l| all_numbers_u64(l)[0])
        .collect_vec();

    let gen1 = Generator::new(16807, 1, nums[0]);
    let gen2 = Generator::new(48271, 1, nums[1]);

    let mask = u16::MAX as u64;

    for (_, val1, val2) in izip!(0..40_000_000, gen1, gen2) {
        if val1 & mask == val2 & mask {
            part1 += 1;
        }
    }

    let mut part2 = 0;
    let gen1 = Generator::new(16807, 4, nums[0]);
    let gen2 = Generator::new(48271, 8, nums[1]);

    for (_, val1, val2) in izip!(0..5_000_000, gen1, gen2) {
        if val1 & mask == val2 & mask {
            part2 += 1;
        }
    }

    (part1, part2)
}

#[derive(Debug, Constructor)]
struct Generator {
    factor: u64,
    filter: u64,
    val: u64,
}

impl Iterator for Generator {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.val *= self.factor;
            self.val %= 2147483647;

            if self.val % self.filter == 0 {
                break;
            }
        }
        Some(self.val)
    }
}

#[test]
fn test_example1() {
    let input = r###"
Generator A starts with 65
Generator B starts with 8921
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 588);
    assert_eq!(part2, 309);
}
