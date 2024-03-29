use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use std::collections::HashSet;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 16)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);

    let part1 = simulate(
        &grid,
        Beam::new(CharGridIndexRC::new(0, 0), RCDirection::right()),
    );

    let mut scores = vec![];
    let b = Beam::new;
    let idx = CharGridIndexRC::new;
    for c in 0..grid.width() {
        scores.push(simulate(&grid, b(idx(0, c), RCDirection::down())));
        scores.push(simulate(
            &grid,
            b(idx(grid.height() - 1, c), RCDirection::up()),
        ));
    }
    for r in 0..grid.height() {
        scores.push(simulate(&grid, b(idx(r, 0), RCDirection::right())));
        scores.push(simulate(
            &grid,
            b(idx(r, grid.width() - 1), RCDirection::left()),
        ));
    }
    let part2 = scores.iter().max().unwrap();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn simulate(grid: &CharGrid, starting_beam: Beam) -> usize {
    let mut energized_tiles = HashSet::new();
    let mut q = Search::new_dfs();

    q.push(starting_beam);

    // dbg!(&grid);

    while let Some(next) = q.pop() {
        // dbg!(next);
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
                        q.push_opt(pos.up().map(|p| Beam::new(p, RCDirection::up())));
                        q.push(Beam::new(pos.down(), RCDirection::down()));
                        break;
                    }
                    // otherwise it just behaves like '.'
                }
                '-' => {
                    if next.direction == RCDirection::up() || next.direction == RCDirection::down()
                    {
                        q.push_opt(pos.left().map(|p| Beam::new(p, RCDirection::left())));
                        q.push(Beam::new(pos.right(), RCDirection::right()));
                        break;
                    }
                    // otherwise it just behaves like '.'
                }
                '/' => {
                    if next.direction == RCDirection::right() {
                        q.push_opt(pos.up().map(|p| Beam::new(p, RCDirection::up())));
                    }
                    if next.direction == RCDirection::down() {
                        q.push_opt(pos.left().map(|p| Beam::new(p, RCDirection::left())));
                    }
                    if next.direction == RCDirection::up() {
                        q.push(Beam::new(pos.right(), RCDirection::right()));
                    }
                    if next.direction == RCDirection::left() {
                        q.push(Beam::new(pos.down(), RCDirection::down()));
                    }
                    break;
                }
                '\\' => {
                    if next.direction == RCDirection::left() {
                        q.push_opt(pos.up().map(|p| Beam::new(p, RCDirection::up())));
                    }
                    if next.direction == RCDirection::up() {
                        q.push_opt(pos.left().map(|p| Beam::new(p, RCDirection::left())));
                    }
                    if next.direction == RCDirection::down() {
                        q.push(Beam::new(pos.right(), RCDirection::right()));
                    }
                    if next.direction == RCDirection::right() {
                        q.push(Beam::new(pos.down(), RCDirection::down()));
                    }
                    break;
                }
                _ => panic!("unrecognised tile '{}' at {}", grid[pos], pos),
            }
            pos = pos + next.direction;
        }
    }

    /*
    for (p, c) in grid.enumerate_chars_rc() {
        if p.col == 0 { println!(); }
        if energized_tiles.contains(&p) {
            print!("#");
        } else {
            print!("{c}");
        }
    }
    println!();
    */

    energized_tiles.len()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Constructor, Hash)]
struct Beam {
    start: CharGridIndexRC,
    direction: RCDirection,
}

#[test]
fn test_example1() -> Result<()> {
    let input = r"
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
";
    let result = solve_for(input)?;

    assert_eq!("Part 1: 46 | Part 2: 51", result);
    Ok(())
}
