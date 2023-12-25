use crate::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::HashMap;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 18)?;

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

    dbg!(&nodes, &links);
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
    let res = search.run_single(|s| get_next_steps(s, &links), |s| s.keys == all_keys);

    Ok(format!("steps: {}", res.steps))
}

fn get_next_steps(s: State, links: &HashMap<char, Vec<(char, usize)>>) -> Vec<State> {
    // println!("getting next steps for {:?}", &s);
    let mut result = vec![];

    for &(n, d) in &links[&s.pos] {
        
        // println!("considering {:?}", (n, d));
        if n.is_ascii_lowercase() && !s.keys.contains(&n) {
            // found a new key
            let mut new_keys = s.keys.clone();
            new_keys.push(n);
            new_keys.sort();
            result.push(State::new(s.steps + d, n, new_keys));
        } else if n.is_ascii_uppercase() && !s.keys.contains(&n.to_ascii_lowercase()) {
            // this is a door we don't have the key to yet
            continue;
        } else {
            // it's a door we can open, or a key we've already collected
            // - just go there and see what's next
            // println!(" searching {:?}", &links[&n]);
            result.push(State::new(s.steps + d, n, s.keys.clone()));
        }
    }
    /*
    let mut search = Dijkstra::new(|s: &(char, usize)| s.0);
    search.push((s.pos, 0));
    let result = search.run_multiple(|s| , |(n, d)| {
        (n.is_ascii_lowercase() && !s.keys.contains(&n)) || 
    });
    */
    /*
    while let Some((n, d)) = search.pop() {
        // println!("considering {:?}", (n, d));
        if n.is_ascii_lowercase() && !s.keys.contains(&n) {
            // found a new key
            let mut new_keys = s.keys.clone();
            new_keys.push(n);
            new_keys.sort();
            result.push(State::new(s.steps + d, n, new_keys));
        } else if n.is_ascii_uppercase() && !s.keys.contains(&n.to_ascii_lowercase()) {
            // this is a door we don't have the key to yet
            continue;
        } else if n.is_ascii_uppercase() && n != s.pos {
            // here's a door we can open
            result.push(State::new(s.steps + d, n, s.keys.clone()));
        } else {
            // println!(" searching {:?}", &links[&n]);
            for &(n2, d2) in &links[&n] {
                search.push((n2, d + d2));
            }
        }
    }
    */
    // println!("next steps from {} with keys {:?}: {:?}", s.pos, s.keys, result);
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

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
"###;
    let result = solve_for(input)?;

    assert_eq!("steps: 86", result);
    Ok(())
}

#[test]
fn test_example3() -> Result<()> {
    let input = r###"
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
"###;
    let result = solve_for(input)?;

    assert_eq!("steps: 132", result);
    Ok(())
}

#[test]
fn test_example4() -> Result<()> {
    let input = r###"
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
"###;
    let result = solve_for(input)?;

    assert_eq!("steps: 136", result);
    Ok(())
}

#[test]
fn test_example5() -> Result<()> {
    let input = r###"
########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################
"###;
    let result = solve_for(input)?;

    assert_eq!("steps: 81", result);
    Ok(())
}
