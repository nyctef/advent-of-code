use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::HashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 9)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for<'i>(input: &'i str) -> Result<String> {
    let mut distances: HashMap<&'i str, Vec<(&'i str, usize)>> = HashMap::new();

    for line in input.trim().lines() {
        let (path, distance) = line.split_once(" = ").unwrap();
        let (source, dest) = path.split_once(" to ").unwrap();
        let distance: usize = distance.parse().unwrap();

        distances
            .entry(source)
            .or_default()
            .push((dest, distance));
        distances
            .entry(dest)
            .or_default()
            .push((source, distance));
    }

    let mut all_points: Vec<&'i str> = distances.keys().copied().collect_vec();
    all_points.sort();
    let mut shortest = usize::MAX;
    for starting_point in &all_points {
        let mut search = Dijkstra::new(|s: &State| s.seen.clone());
        search.push(State::new(starting_point, vec![starting_point], 0));

        let res = search.run_single(
            |s| {
                let mut candidates = vec![];
                for &(dest, dist) in &distances[&s.current] {
                    if !s.seen.contains(&dest) {
                        let mut new_seen = s.seen.clone();
                        new_seen.push(dest);
                        new_seen.sort();
                        candidates.push(State::new(dest, new_seen, s.total_distance + dist));
                    }
                }
                // println!("candidates from {:?} : {:?}", s, candidates);
                candidates
            },
            |s| s.seen == all_points,
        );
        if res.total_distance < shortest {
            shortest = res.total_distance;
        }
    }

    let mut longest = 0_usize;
    for starting_point in &all_points {
        let mut search : Search<State<'_>, ()> = Search::new_exhaustive();
        search.push(State::new(starting_point, vec![starting_point], 0));

        // TODO: a run method for search
        while let Some(s) = search.pop() {
            if s.seen == all_points {
                if s.total_distance > longest {
                    longest = s.total_distance
                }
                continue;
            }

            for &(dest, dist) in &distances[&s.current] {
                if !s.seen.contains(&dest) {
                    let mut new_seen = s.seen.clone();
                    new_seen.push(dest);
                    new_seen.sort();
                    search.push(State::new(dest, new_seen, s.total_distance + dist));
                }
            }
        }
    }

    let part1 = shortest;
    let part2 = longest;
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Constructor)]
struct State<'a> {
    current: &'a str,
    seen: Vec<&'a str>,
    total_distance: usize,
}

impl Ord for State<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.total_distance.cmp(&other.total_distance)
    }
}
impl PartialOrd for State<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 605 | Part 2: 982", result);
    Ok(())
}
