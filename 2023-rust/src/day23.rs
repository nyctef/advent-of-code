use std::collections::HashSet;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashSet;

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

    let mut search = ScoredSearch::new_bfs(|s:&State| s.current_pos, |s:&State| 100_000 - s.visited_points.len());
    search.push(State {
        current_pos: start,
        visited_points: FxHashSet::from_iter(vec![start].into_iter()),
    });

    let res = search.run(
        |s| {
            let c = grid[s.current_pos];
            let p = s.current_pos;
            let mut candidates = vec![];
            match c {
                '>' => candidates.push(p.right()),
                'v' => candidates.push(p.down()),
                '.' => candidates.extend(RCDirection::four().into_iter().map(|d| p + d)),
                _ => panic!("unhandled char {}", c),
            }

            candidates.retain(|ca| grid.is_in_bounds(*ca) && grid[*ca] != '#'  && !s.visited_points.contains(ca));

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
        111_111
    );


    // for (p, c) in grid.enumerate_chars_rc() {
    //     if p.col == 0 {
    //         println!()
    //     }

    //     if res.visited_points.contains(&p) {
    //         print!("O");
    //     } else {
    //         print!("{}", c);
    //     }
    // }
    println!();

    // don't count starting point as a step?
    // let part1 = res.visited_points.len() - 1;
    let res = res.iter().exactly_one().unwrap();
    let res = 100_000 - res;
    let part1 = res - 1;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct State {
    current_pos: CharGridIndexRC,
    visited_points: FxHashSet<CharGridIndexRC>,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // we want to minimize points not taken
        let self_score = (150 * 150) - self.visited_points.len();
        let other_score = (150 * 150) - other.visited_points.len();
        self_score.cmp(&other_score)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
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
