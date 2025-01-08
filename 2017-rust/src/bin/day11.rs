use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 11)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (usize, usize) {
    let mut part2 = 0;

    let mut steps = FxHashMap::default();
    for step in input.trim().split(",") {
        *steps.entry(step).or_default() += 1;
        normalize(&mut steps);
        part2 = part2.max(steps.values().sum());
    }

    let part1 = steps.values().sum();
    (part1, part2)
}

fn normalize(steps: &mut FxHashMap<&str, usize>) {
    let dirs = ["n", "ne", "se", "s", "sw", "nw"];
    for (l, mid, r) in dirs.into_iter().circular_tuple_windows() {
        // eg going 3 nw and 3 ne is equivalent to just going 3 n
        let l_steps = *steps.entry(l).or_default();
        let r_steps = *steps.entry(r).or_default();
        if l_steps > 0 && r_steps > 0 {
            let common = l_steps.min(r_steps);
            *steps.entry(l).or_default() -= common;
            *steps.entry(r).or_default() -= common;
            *steps.entry(mid).or_default() += common;
        }
    }

    for (f, b) in [("n", "s"), ("ne", "sw"), ("nw", "se")] {
        // going 5 n and 3 s is equivalent to just going 2 n
        let f_steps = steps[f];
        let b_steps = steps[b];
        if f_steps > 0 && b_steps > 0 {
            let common = f_steps.min(b_steps);
            steps.entry(f).and_modify(|e| *e -= common);
            steps.entry(b).and_modify(|e| *e -= common);
        }
    }
}

#[test]
fn test_example1() {
    let input = r###"
se,sw,se,sw,sw
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 3);
    assert_eq!(part2, 3);
}
