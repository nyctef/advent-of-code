use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
};

use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 17)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    // TODO: maybe a DigitGrid for this puzzle?
    let grid = CharGrid::from_string(input);
    let mut search = ScoredSearch::new_dfs();

    let right_cost: u32 = grid.index_rc(0, 1).to_string().parse().unwrap();
    let down_cost: u32 = grid.index_rc(1, 0).to_string().parse().unwrap();
    // todo: should initial speed be 1 or 2?
    search.push(
        State::new(CharGridIndexRC::new(0, 1), RCDirection::right(), 2),
        right_cost,
    );
    search.push(
        State::new(CharGridIndexRC::new(1, 0), RCDirection::down(), 2),
        down_cost,
    );

    let mut seen_tiles = HashSet::new();
    while let Some((next, next_score)) = search.pop() {
        seen_tiles.insert(next.pos);
        {
            // continue forward
            let n2p = next.pos + next.dir;
            let n2s = next.speed + 1;
            if grid.is_in_bounds(n2p) {
                let n2c: u32 = next_score + grid[n2p].to_string().parse::<u32>().unwrap();
                if n2s < 4 {
                    search.push(State::new(n2p, next.dir, n2s), n2c);
                }
            }
        }
        {
            // turn left
            let n2d = next.dir.counterclockwise();
            let n2p = next.pos + n2d;
            if grid.is_in_bounds(n2p) {
                let n2s = 1;
                let n2c: u32 = next_score + grid[n2p].to_string().parse::<u32>().unwrap();
                search.push(State::new(n2p, n2d, n2s), n2c);
            }
        }
        {
            // turn right
            let n2d = next.dir.clockwise();
            let n2p = next.pos + n2d;
            if grid.is_in_bounds(n2p) {
                let n2s = 1;
                let n2c: u32 = next_score + grid[n2p].to_string().parse::<u32>().unwrap();
                search.push(State::new(n2p, n2d, n2s), n2c);
            }
        }
    }

    for (p, c) in grid.enumerate_chars_rc() {
        if p.col == 0 {
            println!();
        }
        if seen_tiles.contains(&p) {
            print!("#");
        } else {
            print!("{c}");
        }
    }
    println!();

    let best_scores = search.get_scores_matching(|s| {
        s.pos == CharGridIndexRC::new(grid.height() - 1, grid.width() - 1)
    });
    println!("p1a {:?}", best_scores);
    let part1 = best_scores.iter().min().unwrap();
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Constructor)]
struct State {
    pos: CharGridIndexRC,
    dir: RCDirection,
    speed: u8,
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 102 | Part 2: ", result);
    Ok(())
}
