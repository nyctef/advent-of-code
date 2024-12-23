use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 23)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (usize, String) {
    let mut computers = FxHashSet::default();
    let mut connections = FxHashSet::default();
    let mut triples = FxHashSet::default();

    for line in input.trim().lines() {
        let (c1, c2) = line.split_once('-').unwrap();

        computers.insert(c1);
        computers.insert(c2);

        let mut connection = vec![c1, c2];
        connection.sort();
        connections.insert(connection);
    }

    for &c1 in &computers {
        for &c2 in &computers {
            for &c3 in &computers {
                if !(c1.starts_with('t') || c2.starts_with('t') || c3.starts_with('t')) {
                    continue;
                }

                let mut conn1 = vec![c1, c2];
                let mut conn2 = vec![c2, c3];
                let mut conn3 = vec![c3, c1];
                conn1.sort();
                conn2.sort();
                conn3.sort();

                if connections.contains(&conn1)
                    && connections.contains(&conn2)
                    && connections.contains(&conn3)
                {
                    let mut triple = vec![c1, c2, c3];
                    triple.sort();
                    // eprintln!("found triple {:?}", triple);
                    triples.insert(triple);
                }
            }
        }
    }

    // eprintln!("triples {:?}", triples);

    let mut connection_map = FxHashMap::default();
    for conn in connections {
        connection_map
            .entry(conn[0])
            .or_insert(vec![])
            .push(conn[1]);
        connection_map
            .entry(conn[1])
            .or_insert(vec![])
            .push(conn[0]);
    }

    let part1 = triples.len();

    let mut result = vec![];
    bron_kerbosch(
        FxHashSet::default(),
        computers.clone(),
        FxHashSet::default(),
        &connection_map,
        &mut result,
    );

    let mut largest = result
        .into_iter()
        .max_by(|a, b| a.len().cmp(&b.len()))
        .unwrap();

    largest.sort();

    let part2 = largest.iter().join(",");

    (part1, part2)
}

fn bron_kerbosch<'i>(
    r: FxHashSet<&'i str>,
    mut p: FxHashSet<&'i str>,
    mut x: FxHashSet<&'i str>,
    connections: &FxHashMap<&'i str, Vec<&'i str>>,
    result: &mut Vec<Vec<&'i str>>,
) {
    if p.is_empty() && x.is_empty() && r.len() > 3 {
        result.push(Vec::from_iter(r.iter().copied()));
    }

    // since we need to mutate P inside the loop
    let candidates = p.iter().copied().collect_vec();
    for v in candidates {
        let neighbors = FxHashSet::from_iter(connections[v].iter().copied());

        let mut next_r = r.clone();
        next_r.insert(v);
        let next_p = &p & &neighbors;
        let next_x = &x & &neighbors;

        bron_kerbosch(next_r, next_p, next_x, connections, result);

        p.remove(v);
        x.insert(v);
    }
}

#[test]
fn test_example1() {
    let input = r###"
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 7);
    assert_eq!(part2, "co,de,ka,ta");
}
