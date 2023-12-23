use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::{HashMap};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 23)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);
    let start = grid
        .enumerate_chars_rc()
        .filter(|(p, c)| p.row == 0 && c == &'.')
        .map(|(p, _)| p)
        .exactly_one()
        .unwrap();
    let end = grid
        .enumerate_chars_rc()
        .filter(|(p, c)| p.row == grid.height() - 1 && c == &'.')
        .map(|(p, _)| p)
        .exactly_one()
        .unwrap();

    let mut junctions = grid
        .enumerate_chars_rc()
        .filter(|(p, c)| {
            if c == &'#' {
                return false;
            }

            let n_count = RCDirection::four()
                .into_iter()
                .map(|d| *p + d)
                .filter(|p2| grid.is_in_bounds(*p2) && grid[*p2] != '#')
                .count();
            n_count > 2
        })
        .map(|(p, _)| p)
        .collect_vec();
    junctions.push(start);
    junctions.push(end);

    let mut junction_distances: HashMap<CharGridIndexRC, Vec<(CharGridIndexRC, usize)>, _> =
        FxHashMap::default();

    for j in &junctions {
        for (j2, d) in get_neighbor_junction_dists(&grid, &junctions, *j) {
            junction_distances.entry(*j).or_default().push((j2, d));
        }
    }
    dbg!(&junctions, &junction_distances);

    let mut best = 0;

    let mut junction_search = Search::new_bfs();
    junction_search.push((start, 0, vec![]));


    let mut counter:usize = 0;

    while let Some((n, d, seen)) = junction_search.pop() {
        counter += 1;
        if counter % 100_000 == 0 {
            println!("c {} b {} len {}", counter, best, junction_search.len());

        }
        if n == end {
            best = best.max(d);
            continue;
        }
        for (n2, dd) in &junction_distances[&n] {
            if !seen.contains(&n2) {
                let mut new_seen = seen.clone();
                new_seen.push(n2);
                junction_search.push((*n2, d + dd, new_seen));
            }
        }
    }

    let part2 = best;
    Ok(format!("Part 2: {part2}"))
}

fn get_neighbor_junction_dists(
    grid: &CharGrid,
    junctions: &[CharGridIndexRC],
    j: CharGridIndexRC,
) -> Vec<(CharGridIndexRC, usize)> {
    let mut res = vec![];
    let mut seen = FxHashSet::default();
    let mut search = Search::new_bfs();
    search.push((j, 0));
    while let Some((n, d)) = search.pop() {
        if seen.contains(&n) {
            continue;
        }
        seen.insert(n);
        if n != j && junctions.contains(&n) {
            res.push((n, d));
            continue;
        }

        for dir in RCDirection::four() {
            let n2 = n + dir;
            let d2 = d + 1;
            if grid.is_in_bounds(n2) && grid[n2] != '#' {
                search.push((n2, d2));
            }
        }
    }
    res
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct State {
    current_pos: CharGridIndexRC,
    visited_points: FxHashSet<CharGridIndexRC>,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.visited_points == other.visited_points {
            Some(std::cmp::Ordering::Equal)
        } else if self.visited_points.is_superset(&other.visited_points) {
            // less is "better" for our purposes
            Some(std::cmp::Ordering::Less)
        } else if self.visited_points.is_subset(&other.visited_points) {
            Some(std::cmp::Ordering::Greater)
        } else {
            None
        }
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 2: 154", result);
    Ok(())
}
