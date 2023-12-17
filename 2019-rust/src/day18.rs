use crate::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::HashMap;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 1)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let grid = CharGrid::from_string(input);
    let nodes: HashMap<char, CharGridIndexRC> = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| c.is_ascii_alphabetic() || c == &'@')
        .map(|(p, c)| (c, p))
        .collect();
    let mut links: HashMap<char, Vec<(char, usize)>> = HashMap::new();

    for (c, p) in &nodes {
        links.insert(*c, find_neighboring_nodes(&grid, *p));
    }

    // dbg!(&nodes, &links);
    let all_keys = nodes
        .keys()
        .filter(|x| x.is_ascii_lowercase())
        .copied()
        .sorted()
        .collect_vec();
    dbg!(&all_keys);

    let mut search = Dijkstra::new(|s: &State| (s.pos, s.keys.clone()));
    search.push(State::new(0, '@', vec![]));

    // note this ending condition relies on the state's keys being sorted for vec equality
    // making State.keys into a hashset for easier comparison would be nice, but HashSet isn't
    // hashable itself (which breaks the required trait bound for the state key)
    let res = search.run(|s| get_next_steps(s, &links), |s| s.keys == all_keys);

    dbg!(&res);

    let steps = 0;

    Ok(format!("steps: {}", steps))
}

fn get_next_steps(s: State, links: &HashMap<char, Vec<(char, usize)>>) -> Vec<State> {
    // TODO: starting from s.pos, search through links until we find doors
    // we can't open or keys we don't have yet
    let mut result = vec![];
    todo!();
    result
}

// TODO: does it matter that these Eq/PartialEq implementations are inconsistent with Ord/PartialOrd below?
#[derive(Debug, Eq, PartialEq, Clone, Constructor)]
struct State {
    steps: usize,
    pos: char,
    keys: Vec<char>,
}
impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.steps.cmp(&other.steps)
    }
}
impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn find_neighboring_nodes(grid: &CharGrid, p: CharGridIndexRC) -> Vec<(char, usize)> {
    let mut result = vec![];

    let mut search = Search::new_dfs(|x: &(CharGridIndexRC, usize)| x.0);
    println!("starting at {}", p);
    search.push((p, 0));
    while let Some((np, nd)) = search.pop() {
        let c = grid[np];
        if np != p && (c.is_ascii_alphabetic() || c == '@') {
            result.push((c, nd));
            continue;
        }

        for dir in RCDirection::four() {
            let np2 = np + dir;
            // edge
            if !grid.is_in_bounds(np2) {
                continue;
            }
            // wall
            if grid[np2] == '#' {
                continue;
            }
            search.push((np2, nd + 1));
        }
    }

    result
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
#########
#b.A.@.a#
#########
"###;
    let result = solve_for(input)?;

    assert_eq!("steps: 8", result);
    Ok(())
}

