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

        portals.insert(pn, (portal_nodes[0], portal_nodes[1]));
    }

    let start = nodes
        .iter()
        .filter(|n| n.1 == ['A', 'A'])
        .exactly_one()
        .unwrap();
    let end = nodes
        .iter()
        .filter(|n| n.1 == ['Z', 'Z'])
        .exactly_one()
        .unwrap();

    let mut node_distances = HashMap::new();
    for &(p, node) in &nodes {
        let mut search = Search::new_bfs(|s: &(CharGridIndexRC, usize)| s.0);
        search.push((p, 0));
        while let Some((n, d)) = search.pop() {
            if n != p {
                if let Some(other_node) = nodes.iter().find(|(np, _)| np == &n) {
                    node_distances
                        .entry(node)
                        .or_insert(vec![])
                        .push((other_node.1, d));
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

    let mut dijkstra = Dijkstra::new(|s: &State| s.node);
    dijkstra.push(State { node: ['A', 'A'], steps: 0});
    let result = dijkstra.run_single(
        |s| {
            node_distances[&s.node]
                .iter()
                .map(|n2| State {
                    node: n2.0,
                    // TODO: need to distinguish between walking between portals
                    // vs taking a portal (which consumes an extra step)
                    steps: s.steps + n2.1,
                })
                .collect_vec()
        },
        |s| s.node == ['Z', 'Z'],
    );
    let steps = result.steps;
    Ok(format!("steps: {steps}"))
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct State {
    node: [char; 2],
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
