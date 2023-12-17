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
    let mut search = ScoredSearch::new_bfs(|s: &State| (s.pos, s.dir), |s| s.loss);

    search.push(State::new(
        CharGridIndexRC::new(0, 0),
        RCDirection::right(),
        0,
    ));
    search.push(State::new(
        CharGridIndexRC::new(0, 0),
        RCDirection::down(),
        0,
    ));

    let theoretical_max_states = grid.width() * grid.height() * 4 * 10;
    println!("max states: {}", theoretical_max_states);
    let target = CharGridIndexRC::new(grid.height() - 1, grid.width() - 1);
    // it's going to be hard to get worse than going straight along the top
    // edge, then straight along the right edge and hitting a 9 on every tile.
    //
    // the optimal solution will weave back and forth a bit, so it covers
    // more tiles than this worst case, but the only reason to do that
    // is because it has a lower overall cost than just going directly.
    //
    // this upper bound really helps speed up DFS since it means we can reject
    // tons and tons of paths that are worse than this bound, but it doesn't
    // help so much with a BFS approach.
    let probable_limit = ((grid.width() + grid.height()) * 9) as u32;
    println!("probable max score: {}", probable_limit);

    let bests = search.run(
        |n| next_candidates(n, &grid),
        |s| s.pos == target,
        probable_limit,
    );
    let best = bests.iter().exactly_one().unwrap();

    Ok(format!("best: {}", best))
}

fn next_candidates(current: State, grid: &CharGrid) -> Vec<State> {
    let mut candidates = vec![];
    let mut cost: u32 = current.loss;
    let mut current_tile = current.pos;
    for speed in 1..=10 {
        current_tile = current_tile + current.dir;
        if !grid.is_in_bounds(current_tile) {
            break;
        }
        cost += grid[current_tile].to_digit(10).unwrap();
        if speed >= 4 {
            // turn left
            let next_dir = current.dir.counterclockwise();
            candidates.push(State::new(current_tile, next_dir, cost));
            // turn right
            let next_dir = current.dir.clockwise();
            candidates.push(State::new(current_tile, next_dir, cost));
        }
    }
    // dbg!(&current.pos, &current_tile, &cost, &candidates);
    candidates
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Constructor)]
struct State {
    pos: CharGridIndexRC,
    dir: RCDirection,
    loss: u32,
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
