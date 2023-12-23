use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::{HashMap, HashSet};

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

    let mut search = ScoredSearch::new_bfs(
        |s: &State| s.current_pos,
        |s: &State| -(s.visited_points.len() as isize),
    );
    search.push(State {
        current_pos: start,
        visited_points: FxHashSet::from_iter(vec![start].into_iter()),
    });

    let (bests, res) = search.run(
        |s| {
            let c = grid[s.current_pos];
            let p = s.current_pos;
            let mut candidates = vec![];
            match c {
                // '>' => candidates.push(p.right()),
                // 'v' => candidates.push(p.down()),
                _ => candidates.extend(RCDirection::four().into_iter().map(|d| p + d)),
                // _ => panic!("unhandled char {}", c),
            }

            candidates.retain(|ca| {
                grid.is_in_bounds(*ca) && grid[*ca] != '#' && !s.visited_points.contains(ca)
            });

            candidates
                .into_iter()
                .map(|ca| {
                    // TODO: is there a way to do this in one expression using .union() or
                    // something?
                    let mut new_visited_points = s.visited_points.clone();
                    new_visited_points.insert(ca);
                    State {
                        current_pos: ca,
                        visited_points: new_visited_points,
                    }
                })
                .collect_vec()
        },
        |s| s.current_pos == end,
        0,
    );

    for (p, c) in grid.enumerate_chars_rc() {
        if p.col == 0 {
            println!()
        }

        if res.as_ref().unwrap().visited_points.contains(&p) {
            print!("O");
        } else {
            print!("{}", c);
        }
    }
    println!();

    // don't count starting point as a step?
    // let part1 = res.visited_points.len() - 1;
    let res = bests.iter().exactly_one().unwrap();
    let res = -res;
    let part1 = res - 1;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
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

    assert_eq!("Part 1: 94 | Part 2: ", result);
    Ok(())
}
