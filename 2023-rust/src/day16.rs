use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::{HashSet, VecDeque};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 16)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);

    let mut seen_beams = HashSet::new();
    let mut energized_tiles = HashSet::new();
    let mut q = VecDeque::new();

    let starting_beam = Beam::new(CharGridIndexRC::new(0, 0), RCDirection::right());
    seen_beams.insert(starting_beam);
    q.push_front(starting_beam);

    let q_push = |q: &mut VecDeque<_>, seen_beams: &HashSet<_>, b| {
        if !seen_beams.contains(&b) {
            q.push_front(b);
        }
    };

    dbg!(&grid);

    while let Some(next) = q.pop_front() {
        // dbg!(next);
        seen_beams.insert(next);
        let mut pos = next.start;
        loop {
            if !grid.is_in_bounds(pos) {
                break;
            }
            energized_tiles.insert(pos);
            match grid[pos] {
                '.' => {}
                '|' => {
                    if next.direction == RCDirection::left()
                        || next.direction == RCDirection::right()
                    {
                        if pos.up().is_some() {
                            q_push(
                                &mut q,
                                &seen_beams,
                                Beam::new(pos.up().unwrap(), RCDirection::up()),
                            );
                        }
                        q_push(
                            &mut q,
                            &seen_beams,
                            Beam::new(pos.down(), RCDirection::down()),
                        );
                        break;
                    }
                    // otherwise it just behaves like '.'
                }
                '-' => {
                    if next.direction == RCDirection::up() || next.direction == RCDirection::down()
                    {
                        if pos.left().is_some() {
                            q_push(
                                &mut q,
                                &seen_beams,
                                Beam::new(pos.left().unwrap(), RCDirection::left()),
                            );
                        }
                        q_push(
                            &mut q,
                            &seen_beams,
                            Beam::new(pos.right(), RCDirection::right()),
                        );
                        break;
                    }
                    // otherwise it just behaves like '.'
                }
                '/' => {
                    if next.direction == RCDirection::right() {
                        if pos.up().is_some() {
                            q_push(
                                &mut q,
                                &seen_beams,
                                Beam::new(pos.up().unwrap(), RCDirection::up()),
                            );
                        }
                    }
                    if next.direction == RCDirection::down() {
                        if pos.left().is_some() {
                            q_push(
                                &mut q,
                                &seen_beams,
                                Beam::new(pos.left().unwrap(), RCDirection::left()),
                            );
                        }
                    }
                    if next.direction == RCDirection::up() {
                        q_push(
                            &mut q,
                            &seen_beams,
                            Beam::new(pos.right(), RCDirection::right()),
                        );
                    }
                    if next.direction == RCDirection::left() {
                        q_push(
                            &mut q,
                            &seen_beams,
                            Beam::new(pos.down(), RCDirection::down()),
                        );
                    }
                    break;
                }
                '\\' => {
                    if next.direction == RCDirection::left() {
                        if pos.up().is_some() {
                            q_push(
                                &mut q,
                                &seen_beams,
                                Beam::new(pos.up().unwrap(), RCDirection::up()),
                            );
                        }
                    }
                    if next.direction == RCDirection::up() {
                        if pos.left().is_some() {
                            q_push(
                                &mut q,
                                &seen_beams,
                                Beam::new(pos.left().unwrap(), RCDirection::left()),
                            );
                        }
                    }
                    if next.direction == RCDirection::down() {
                        q_push(
                            &mut q,
                            &seen_beams,
                            Beam::new(pos.right(), RCDirection::right()),
                        );
                    }
                    if next.direction == RCDirection::right() {
                        q_push(
                            &mut q,
                            &seen_beams,
                            Beam::new(pos.down(), RCDirection::down()),
                        );
                    }
                    break;
                }
                _ => panic!("unrecognised tile '{}' at {}", grid[pos], pos),
            }
            pos = pos + next.direction;
        }
    }

    for (p, c) in grid.enumerate_chars_rc() {
        if p.col == 0 { println!(); }
        if energized_tiles.contains(&p) {
            print!("#");
        } else {
            print!("{c}");
        }
    }
    println!();

    let part1 = energized_tiles.len();
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Constructor, Hash)]
struct Beam {
    start: CharGridIndexRC,
    direction: RCDirection,
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 46 | Part 2: ", result);
    Ok(())
}
