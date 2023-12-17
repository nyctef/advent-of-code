use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

use std::cmp::{self, Ordering};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 17)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    // TODO: maybe a DigitGrid for this puzzle?
    let grid = CharGrid::from_string(input);
    let mut search = ScoredSearch::new_bfs(|s: &State| (s.pos, s.dir, s.speed), |s| s.loss);

    let right_cost: u32 = grid.index_rc(0, 1).to_digit(10).unwrap();
    let down_cost: u32 = grid.index_rc(1, 0).to_digit(10).unwrap();
    search.push(State::new(
        CharGridIndexRC::new(0, 1),
        RCDirection::right(),
        right_cost,
        1,
    ));
    search.push(State::new(
        CharGridIndexRC::new(1, 0),
        RCDirection::down(),
        down_cost,
        1,
    ));

    let theoretical_max_states = grid.width() * grid.height() * 4 * 10;
    println!("max states: {}", theoretical_max_states);
    let target = CharGridIndexRC::new(grid.height() - 1, grid.width() - 1);
    let probable_limit = ((grid.width() + grid.height()) * 9) as u32;
    println!("probable max score: {}", probable_limit);

    let bests = search.run(
        |n| next_candidates(n, &grid),
        |s| s.pos == target && s.speed >= 4,
        probable_limit,
    );
    let best = bests.iter().exactly_one().unwrap();

    Ok(format!("best: {}", best))
}

fn next_candidates(next: State, grid: &CharGrid) -> Vec<State> {
    let mut candidates = vec![];
    if next.speed < 10 {
        // continue forward
        let n2p = next.pos + next.dir;
        let n2s = next.speed + 1;
        if grid.is_in_bounds(n2p) {
            let n2c: u32 = next.loss + grid[n2p].to_digit(10).unwrap();
            candidates.push(State::new(n2p, next.dir, n2c, n2s));
        }
    }
    if next.speed >= 4 {
        // turn left
        let n2d = next.dir.counterclockwise();
        let n2p = next.pos + n2d;
        if grid.is_in_bounds(n2p) {
            let n2s = 1;
            let n2c: u32 = next.loss + grid[n2p].to_digit(10).unwrap();
            candidates.push(State::new(n2p, n2d, n2c, n2s));
        }
    }
    if next.speed >= 4 {
        // turn right
        let n2d = next.dir.clockwise();
        let n2p = next.pos + n2d;
        if grid.is_in_bounds(n2p) {
            let n2s = 1;
            let n2c: u32 = next.loss + grid[n2p].to_digit(10).unwrap();
            candidates.push(State::new(n2p, n2d, n2c, n2s));
        }
    }
    candidates
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Constructor)]
struct State {
    pos: CharGridIndexRC,
    dir: RCDirection,
    loss: u32,
    speed: u8,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self.pos != other.pos || self.dir != other.dir {
            // can't compare two states that are in different positions
            return None;
        }

        /*             loss:
                       < = >
                     +--------
            speed: < | < < #
                   = | < = >
                   > | # > >

            - if one/both is (greater/less) or equal, and at least one
              is strictly (greater/less), then we're (greater/less)
            - if both are equal then we're equal
            - if one is (greater/less) and the other is (less/greater),
              then we can't exactly compare these two states
        */

        if self.speed == other.speed && self.loss == other.loss {
            return Some(Ordering::Equal);
        }
        if (self.speed < other.speed && self.loss > other.loss)
            || (self.speed > other.speed && self.loss < other.loss)
        {
            // either state could be better, so can't compare
            return None;
        }
        // by now we know we're not equal and we're not incomparable,
        // so if one or the other property is greater/less then
        // that's the answer
        if self.speed < other.speed || self.loss < other.loss {
            return Some(Ordering::Less);
        }
        if self.speed > other.speed || self.loss > other.loss {
            return Some(Ordering::Greater);
        }
        // not sure if we ever hit this case
        panic!("failed to compare states")
    }
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

    assert_eq!("best: 94", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
111111111111
999999999991
999999999991
999999999991
999999999991
"###;

    let result = solve_for(input)?;

    assert_eq!("best: 71", result);
    Ok(())
}
