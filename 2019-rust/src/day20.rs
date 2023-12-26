use std::collections::HashMap;

use crate::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 20)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let lines = input
        .lines()
        .filter(|l| l != &"")
        .map(|l| l.to_owned())
        .collect_vec();
    let grid = CharGrid::new(lines);

    // first find the portal locations
    let mut nodes = vec![];
    let mut points_to_nodes = HashMap::new();
    for (p, c) in grid.enumerate_chars_rc() {
        if c != '.' {
            continue;
        }

        for d in RCDirection::four() {
            let p2 = p + d;
            let c2 = grid[p2];

            if c2.is_ascii_uppercase() {
                let p3 = p2 + d;
                let c3 = grid[p3];
                assert!(c3.is_ascii_uppercase());

                let mut name = [c2, c3];
                if d == RCDirection::left() || d == RCDirection::up() {
                    name.reverse();
                }
                nodes.push((p, name));
                points_to_nodes.insert(p, name);
            }
        }
    }

    let portal_names = nodes.iter().map(|(_, name)| name).unique();

    let mut portals = HashMap::new();

    for &pn in portal_names {
        if pn == ['A', 'A'] || pn == ['Z', 'Z'] {
            continue;
        }

        let portal_nodes = nodes.iter().filter(|n| n.1 == pn).collect_vec();
        assert!(portal_nodes.len() == 2);

        portals.insert(portal_nodes[0].0, portal_nodes[1].0);
        portals.insert(portal_nodes[1].0, portal_nodes[0].0);
    }

    let start = nodes
        .iter()
        .filter(|n| n.1 == ['A', 'A'])
        .exactly_one()
        .unwrap()
        .0;
    let end = nodes
        .iter()
        .filter(|n| n.1 == ['Z', 'Z'])
        .exactly_one()
        .unwrap()
        .0;

    let mut node_distances = HashMap::new();
    for &(p, node) in &nodes {
        let mut search = Search::new_bfs(|s: &(CharGridIndexRC, usize)| s.0);
        search.push((p, 0));
        while let Some((n, d)) = search.pop() {
            if n != p {
                if let Some(other_node) = nodes.iter().find(|(np, _)| np == &n) {
                    node_distances
                        .entry(p)
                        .or_insert(vec![])
                        .push((other_node, d));
                    continue;
                }
            }

            for dir in RCDirection::four() {
                let n2 = n + dir;
                if grid[n2] != '.' {
                    continue;
                }
                search.push((n2, d + 1));
            }
        }
    }

    dbg!(&nodes, &node_distances);

    let mut dijkstra = Dijkstra::new(|s: &State| s.pos);
    dijkstra.push(State {
        pos: start,
        steps: 0,
    });
    let result = dijkstra.run_single(
        |s| {
            let mut res = vec![];

            if let Some(&p) = portals.get(&s.pos) {
                res.push(State {
                    pos: p,
                    steps: s.steps + 1,
                });
            }

            let current_node = points_to_nodes[&s.pos];
            for &(p, d) in &node_distances[&s.pos] {
                let p2 = p.0;
                res.push(State {
                    pos: p2,
                    steps: s.steps + d,
                });
            }

            res
        },
        |s| s.pos == end,
    );
    let steps = result.steps;
    Ok(format!("steps: {steps}"))
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct State {
    pos: CharGridIndexRC,
    steps: usize,
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

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       
"###;
    let result = solve_for(input)?;

    assert_eq!("steps: 23", result);
    Ok(())
}
