use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 14)?;

    let result = solve_for(&input, 2503)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, time: usize) -> Result<String> {
    // (speed, time, rest)
    let reindeer = input
        .trim()
        .lines()
        .map(|l| {
            all_numbers_usize(l)
                .into_iter()
                .collect_tuple::<(_, _, _)>()
                .unwrap()
        })
        .collect_vec();

    let mut part1 = 0;
    for &(speed, fast_time, rest_time) in &reindeer {
        let mut time_left = time;
        let mut distance = 0;
        loop {
            let moving_time = fast_time.min(time_left);
            distance += speed * moving_time;
            time_left = time_left.saturating_sub(moving_time);
            time_left = time_left.saturating_sub(rest_time);

            if time_left <= 0 {
                break;
            }
        }

        if distance > part1 {
            part1 = distance;
        }
    }

    let mut r_distances = Vec::new();
    let mut r_scores = Vec::new();
    for _ in 0..reindeer.len() {
        r_distances.push(0);
        r_scores.push(0);
    }

    for t in 0..time {
        for r in 0..reindeer.len() {
            let (speed, fast_time, rest_time) = reindeer[r];
            let is_moving = t.rem_euclid(fast_time + rest_time) < fast_time;
            if is_moving {
                r_distances[r] += speed;
            }
        }

        let lead = r_distances.iter().max().unwrap();
        // any reindeer tied for the lead gets a point
        for r in 0..reindeer.len() {
            if r_distances[r] == *lead {
                r_scores[r] += 1;
            }
        }
    }

    let part2 = r_scores.iter().max().unwrap();

    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
"###;
    let result = solve_for(input, 1000)?;

    assert_eq!("Part 1: 1120 | Part 2: 689", result);
    Ok(())
}
