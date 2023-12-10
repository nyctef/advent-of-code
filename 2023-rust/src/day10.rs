use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter,
};

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 10)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);

    let (start_pos, _) = grid
        .enumerate_chars_rc()
        .filter(|(i, c)| *c == 'S')
        .exactly_one()
        .expect("S");

    let start_neighbor_candidates = grid.enumerate_4_neighbors(start_pos).collect_vec();
    // dbg!(&start_pos);
    // dbg!(&neighbors);

    let mut start_neighbors = vec![];
    for (n, nc) in start_neighbor_candidates {
        if get_connections(&grid, n).iter().contains(&start_pos) {
            start_neighbors.push(n);
        }
    }
    assert!(
        start_neighbors.len() == 2,
        "the puzzle says that the starting position isn't ambiguous"
    );

    let mut seen = HashSet::new();
    seen.insert(start_pos);
    let mut queue = VecDeque::new();
    queue.push_back(start_neighbors[0]);
    queue.push_back(start_neighbors[1]);

    while !queue.is_empty() {
        let next = queue.pop_front().unwrap();
        seen.insert(next);
        let connections = &get_connections(&grid, next);
        let next_connection = connections
            .iter()
            .filter(|x| !seen.contains(x))
            .at_most_one()
            .expect("should be one seen and one unseen");
        if let Some(nc) = next_connection {
            queue.push_back(*nc);
        } else {
            break;
        }
    }

    println!("{} {}", seen.len(), seen.len() / 2);

    let part1 = seen.len() / 2;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn get_connections(grid: &CharGrid, pos: CharGridIndexRC) -> Vec<CharGridIndexRC> {
    let char = grid.index(pos);
    match char {
        '.' => iter::empty().collect_vec(),
        '|' => [pos.up(), Some(pos.down())]
            .iter()
            .filter_map(|x| *x)
            .collect_vec(),
        '-' => [pos.left(), Some(pos.right())]
            .iter()
            .filter_map(|x| *x)
            .collect_vec(),
        'L' => [pos.up(), Some(pos.right())]
            .iter()
            .filter_map(|x| *x)
            .collect_vec(),
        'J' => [pos.up(), pos.left()]
            .iter()
            .filter_map(|x| *x)
            .collect_vec(),
        '7' => [pos.left(), Some(pos.down())]
            .iter()
            .filter_map(|x| *x)
            .collect_vec(),
        'F' => [pos.down(), pos.right()].iter().map(|x| *x).collect_vec(),
        _ => panic!("don't know how to handle char {}", char),
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 4 | Part 2: ", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 8 | Part 2: ", result);
    Ok(())
}
