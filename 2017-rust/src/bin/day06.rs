use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 6)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let nums = all_numbers(input);
    let mut part1 = 0;
    let part2;

    let mut seen = FxHashMap::default();
    let mut memory = nums.clone();
    let len = memory.len();
    loop {
        // eprintln!("{:?}", memory);
        if let Some(last_seen) = seen.insert(memory.clone(), part1) {
            part2 = part1 - last_seen;
            break;
        }
        let (i, x) = memory
            .iter()
            .copied() // since max_by will return the last item in case of a tie, and we want the first
            .enumerate()
            .rev()
            .max_by_key(|(_, x)| *x)
            .unwrap();

        // eprintln!("max: {x} at {i}");
        memory[i] = 0;
        for inext in 0..x {
            let inext = inext as usize;
            memory[(i + inext + 1) % len] += 1;
        }
        part1 += 1;
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
0 2 7 0
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 5);
    assert_eq!(part2, 4);
}
