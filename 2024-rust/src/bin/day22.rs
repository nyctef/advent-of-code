use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 22)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let seeds = input
        .trim()
        .lines()
        .map(|l| all_numbers_u64(l)[0])
        .collect_vec();

    let mut part1 = 0;
    let mut total_sequences_to_bananas = FxHashMap::default();
    let count = 2000;

    for seed in seeds {
        eprintln!("seed: {}", seed);
        let mut sequences_to_bananas = FxHashMap::default();
        let mut prices = vec![0_i8; count + 1];
        let mut diffs = vec![0_i8; count + 1];
        prices[0] = (seed % 10) as i8;

        let mut x = seed;
        for i in 0..count {
            x = next(x);
            prices[i + 1] = (x % 10) as i8;
            diffs[i + 1] = prices[i + 1] - prices[i];
        }
        // dbg!(&prices, &diffs);
        //
        for i in 4..count {
            let sequence = &diffs[i-3..=i];
            let sequence = sequence.to_vec();
            let bananas = prices[i];

            // we only store the first instance of each banana per seed
            sequences_to_bananas.entry(sequence).or_insert(bananas);

        }

        for (seq, banans) in sequences_to_bananas {
            *total_sequences_to_bananas.entry(seq).or_insert(0) += banans as u64;

        }

        // dbg!(&sequences_to_bananas);

        part1 += x;
    }

    let max = total_sequences_to_bananas.iter().max_by(|a, b| a.1.cmp(&b.1)).unwrap();
    dbg!(&max);
    let part2 = *max.1;

    (part1, part2)
}

fn next(mut x: u64) -> u64 {
    x = x ^ (x * 64);
    x = x % 16777216;
    x = x ^ (x / 32);
    x = x % 16777216;
    x = x ^ (x * 2048);
    x = x % 16777216;
    x
}

#[test]
fn test_example1() {
    let input = r###"
1
10
100
2024
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 37327623);
    assert_eq!(part2, 23);
}

#[test]
fn test_example2() {
    let input = r###"
1
2
3
2024
"###;
    let (part1, part2) = solve_for(input);

    // assert_eq!(part1, 37327623);
    assert_eq!(part2, 23);
}
