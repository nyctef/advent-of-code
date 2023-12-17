use std::{
    cmp::{self, Ordering},
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

    let theoretical_max_states = grid.width() * grid.height() * 4 * 3;
    println!("max states: {}", theoretical_max_states);
    let mut count: u64 = 0;
    let target = CharGridIndexRC::new(grid.height() - 1, grid.width() - 1);
    let probable_limit = ((grid.width() + grid.height()) * 9) as u32;
    println!("probable max score: {}", probable_limit);
    let mut best = probable_limit;

    let mut bests: HashMap<(CharGridIndexRC, RCDirection), Vec<(u8, u32)>> = HashMap::new();

    while let Some(next) = search.pop() {
        if next.pos == target && next.speed >= 4 && next.loss < best {
            println!("reached end with speed {} and loss {}", next.speed, next.loss);
            println!("dir: {}", next.dir);
            best = next.loss;
        }
        if count % 1_000_000 == 0 {
            println!("{} best: {}", search.debug_info(), best);
        }
        count += 1;
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

        // prioritise getting down to the end first, so that we have a baseline
        // score to eliminate other paths with
        candidates.sort_by_key(|c| cmp::Reverse((c.pos.row, c.pos.col)));
        for c in candidates {
            let min_cost_to_end =
                c.loss as usize + RCDirection::from_to(&c.pos, &target).manhattan_abs();
            if min_cost_to_end > best as usize {
                // assuming every tile between here and the target was 1, we still wouldn't
                // be able to beat the current best score
                continue;
            }

            let e = bests.entry((c.pos, c.dir)).or_default();
            if e.iter().any(|x|  x.0 == c.speed && x.1 <= c.loss) {
                // we've already reached this position with an equal or better score, so skip
                continue;
            } else {
                let value = (c.speed, c.loss);
                e.push(value);
                // only retain scores that aren't strictly worse than the one we've just added
                e.retain(|e2| !(e2.0 == value.0 && e2.1 > value.1));
                assert!(!e.is_empty());
            }
            search.push(c);
        }
    }

    let mut next_best = best;
    let mut current_tile = target;
    let mut path_tiles = HashSet::new();
    let mut tile_losses_per_tile: HashMap<CharGridIndexRC, Vec<u32>> = HashMap::new();
    for (bk, bv) in bests {
        let e = tile_losses_per_tile.entry(bk.0).or_default();
        e.extend(bv.iter().map(|x| x.1));
    }

    'outer: while next_best > 0 {
        path_tiles.insert(current_tile);
        let this_tile_score: u32 = grid[current_tile].to_digit(10).unwrap();
        println!(
            "ct {:?} ({}) with overall current score {}",
            &current_tile, &this_tile_score, &next_best
        );
        println!(
            "looking for a tile with score {}",
            next_best - this_tile_score
        );
        for (n, _) in grid.enumerate_4_neighbors(current_tile) {
            let tile_losses = tile_losses_per_tile
                .get(&n)
                .into_iter()
                .flatten()
                .collect_vec();
            println!(
                "considering tile {:?} with best losses {:?}",
                &n, &tile_losses
            );
            for l in tile_losses {
                if *l == next_best - this_tile_score {
                    current_tile = n;
                    next_best = *l;
                    continue 'outer;
                }
            }
        }
        // TODO: shouldn't hit this
        break 'outer;
    }

    for (p, c) in grid.enumerate_chars_rc() {
        if p.col == 0 {
            println!();
        }
        if path_tiles.contains(&p) {
            print!("#");
        } else if tile_losses_per_tile.contains_key(&p) {
            print!("?");
        } else {
            print!("{c}");
        }
    }
    println!();

    println!("final best: {}", best);
    Ok(format!("best: {}", best))
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
