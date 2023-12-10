use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::{
    collections::{HashSet, VecDeque},
    iter,
};

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
        .filter(|(_i, c)| *c == 'S')
        .exactly_one()
        .expect("S");

    let start_neighbor_candidates = grid.enumerate_4_neighbors(start_pos).collect_vec();
    // dbg!(&start_pos);
    // dbg!(&neighbors);

    let mut start_neighbors = vec![];
    for (n, _nc) in start_neighbor_candidates {
        if get_connections(&grid, n).iter().contains(&start_pos) {
            start_neighbors.push(n);
        }
    }
    assert!(
        start_neighbors.len() == 2,
        "the puzzle says that the starting position isn't ambiguous"
    );

    let mut loop_pipes = HashSet::new();
    loop_pipes.insert(start_pos);
    let mut queue = VecDeque::new();
    queue.push_back(start_neighbors[0]);
    queue.push_back(start_neighbors[1]);

    while !queue.is_empty() {
        let next = queue.pop_front().unwrap();
        loop_pipes.insert(next);
        let connections = &get_connections(&grid, next);
        let next_connection = connections
            .iter()
            .filter(|x| !loop_pipes.contains(x))
            .at_most_one()
            .expect("should be one seen and one unseen");
        if let Some(nc) = next_connection {
            queue.push_back(*nc);
        } else {
            break;
        }
    }

    let mut outside1 = flood_fill_4(&grid, CharGridIndexRC::new(0, 0), &loop_pipes);
    let outside2 = flood_fill_4(
        &grid,
        CharGridIndexRC::new(0, grid.width() - 1),
        &loop_pipes,
    );
    outside1.extend(outside2);

    // let inside1 = flood_fill_4(&grid, CharGridIndexRC::new(70, 70), &loop_pipes);
    // outside1.extend(inside1);

    print_loop_chars(grid, &loop_pipes, &outside1);

    // println!("{} {}", seen.len(), seen.len() / 2);

    let part1 = loop_pipes.len() / 2;
    let part2 = "";
    // Ok(format!("Part 1: {part1} | Part 2: {part2}"))
    Ok("".to_string())
}

fn flood_fill_4(
    grid: &CharGrid,
    start: CharGridIndexRC,
    boundaries: &HashSet<CharGridIndexRC>,
) -> HashSet<CharGridIndexRC> {
    let mut result = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(start);

    while let Some(next) = queue.pop_front() {
        // println!("q{} r{}", queue.len(), result.len());
        result.insert(next);
        for (n, _) in grid.enumerate_4_neighbors(next) {
            if grid.is_in_bounds(n) && !result.contains(&n) && !boundaries.contains(&n) {
                queue.push_front(n);
            }
        }
    }

    result
}

fn print_loop_chars(
    grid: CharGrid,
    seen: &HashSet<CharGridIndexRC>,
    outside: &HashSet<CharGridIndexRC>,
) {
    for (i, c) in grid.enumerate_chars_rc() {
        if i.col == 0 {
            print!("\n")
        }
        if outside.contains(&i) {
            print!(" ");
        } else if seen.contains(&i) {
            print!("\x1b[7m");
            print!("{}", c);
            print!("\x1b[0m");
        } else {
            print!("{}", c);
        }
    }
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

    assert_eq!("Part 1: 4 | Part 2: 1", result);
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

    assert_eq!("Part 1: 8 | Part 2: 1", result);
    Ok(())
}

#[test]
fn test_example3() -> Result<()> {
    let input = r###"
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 23 | Part 2: 4", result);
    Ok(())
}

#[test]
fn test_example4() -> Result<()> {
    let input = r###"
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 70 | Part 2: 8", result);
    Ok(())
}

#[test]
fn test_example5() -> Result<()> {
    let input = r###"
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 80 | Part 2: 10", result);
    Ok(())
}
