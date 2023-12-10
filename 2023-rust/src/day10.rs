use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter,
};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 10)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut grid = CharGrid::from_string(input);

    dbg!(grid.height(), grid.width());

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

    // TODO: detect what character S should be replaced with
    // match (
    //     start_neighbors[0].row - start_pos.row,
    //     start_neighbors[0].col - start_pos.col,
    //     start_neighbors[1].row - start_pos.row,
    //     start_neighbors[1].col - start_pos.col,
    // ) {
    //     (a, b, c, d) => panic!("{:?} {:?} {a} {b} {c} {d}", start_pos, start_neighbors),
    // }
    // hacks!
    if grid.height() > 100 {
        grid.set_index_rc(start_pos, '7');
    } else {
        grid.set_index_rc(start_pos, 'F')
    }

    let mut loop_pipes = HashSet::new();
    loop_pipes.insert(start_pos);
    let mut queue = VecDeque::new();
    // direction here
    let mut entrance_directions: HashMap<CharGridIndexRC, RCDirection> = HashMap::new();
    let mut exit_directions: HashMap<CharGridIndexRC, RCDirection> = HashMap::new();

    // TODO: correctly dectect which direction gives is a counterclockwise winding
    // queue.push_back(start_neighbors[0]);
    if grid.height() > 100 {
        // queue.push_back(start_pos.left().unwrap());
        queue.push_back(start_pos.down());
        // exit_directions.insert(start_pos, RCDirection::left());
        // exit_directions.insert(start_pos.down(), RCDirection::up());
        // entrance_directions.insert(start_pos, RCDirection::up());
        // entrance_directions.insert(start_pos.left().unwrap(), RCDirection::left());
    } else {
        queue.push_back(start_pos.down());
        exit_directions.insert(start_pos, RCDirection::down());
        exit_directions.insert(start_pos.right(), RCDirection::left());
        entrance_directions.insert(start_pos, RCDirection::left());
        entrance_directions.insert(start_pos.down(), RCDirection::down());
    }

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
            entrance_directions.insert(*nc, RCDirection::from_to(&next, nc));
            exit_directions.insert(next, RCDirection::from_to(&next, nc));
            queue.push_back(*nc);
        } else {
            break;
        }
    }

    // dbg!(&entrance_directions);

    let mut outside1 = flood_fill_4(&grid, CharGridIndexRC::new(0, 0), &loop_pipes);
    let outside2 = flood_fill_4(
        &grid,
        CharGridIndexRC::new(0, grid.width() - 1),
        &loop_pipes,
    );
    outside1.extend(outside2);

    // let inside1 = flood_fill_4(&grid, CharGridIndexRC::new(70, 70), &loop_pipes);
    // outside1.extend(inside1);

    print_loop_chars(&grid, &loop_pipes, &outside1);

    // println!("{} {}", seen.len(), seen.len() / 2);

    let mut contained_count = 0;
    for (p, c) in grid.enumerate_chars_rc() {
        if loop_pipes.contains(&p) {
            continue;
        }

        // flood fill to the nearest loop pipe
        let (colliding_tile, loop_pipe) = flood_fill_4_until_collision(&grid, p, &loop_pipes);
        // TODO: this can probably be a index.sub() impl or something
        let collision_direction = RCDirection::from_to(&colliding_tile, &loop_pipe);
        let exit_direction = exit_directions.get(&loop_pipe);
        let entrance_direction = entrance_directions.get(&loop_pipe);
        // .unwrap_or_else(|| panic!("failed to get loop direction for {}", loop_pipe));

        println!(
            "starting at {}, filled to {} -> {} | with collision direction {} exit direction {:?}",
            p, colliding_tile, loop_pipe, &collision_direction, &exit_direction
        );
        if exit_direction.is_some()
            && *exit_direction.unwrap() == collision_direction.counterclockwise()
        {
            println!("  -> inside!");
            contained_count += 1;
            // break;
        } else if entrance_direction.is_some()
            && *entrance_direction.unwrap() == collision_direction.counterclockwise()
        {
            println!("  -> inside!");
            contained_count += 1;
            // break;
        }

        // for the collision between the flood fill and the loop pipe,
        // compare the
    }

    let part1 = loop_pipes.len() / 2;
    let part2 = contained_count;
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

// TODO: move to grid struct
// TODO: replace boundary set with a predicate?
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

fn flood_fill_4_until_collision(
    grid: &CharGrid,
    start: CharGridIndexRC,
    target: &HashSet<CharGridIndexRC>,
) -> (CharGridIndexRC, CharGridIndexRC) {
    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(start);

    while let Some(next) = queue.pop_front() {
        // println!("q{} r{}", queue.len(), result.len());
        seen.insert(next);
        for (n, _) in grid.enumerate_4_neighbors(next) {
            if target.contains(&n) {
                return (next, n);
            }

            if grid.is_in_bounds(n) && !seen.contains(&n) {
                queue.push_front(n);
            }
        }
    }

    panic!("collision not found")
}

#[allow(dead_code)]
fn print_loop_chars(
    grid: &CharGrid,
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
    println!()
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
