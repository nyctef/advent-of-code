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

/// https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
///
/// Bron-Kerbosch is an algorithm for finding "cliques" (fully-connected subgraphs).
///
/// In particular Bron_kerbosch finds all "maximal" cliques - cliques that could not be expanded
/// any further by adding a neighboring vertex. For today's puzzle, we care about the largest
/// maximal clique - called the "maximum" clique, so we'll look through the results to find that
/// once we're done with this algorithm.
///
/// A simpler algorithm for finding maximal cliques could look like the following:
/// - for each vertex in the graph
///   - put that vertex into a set R
///   - loop:
///     - for all neighbors of vertices in R
///       - if a neighbor added to R would keep R as a clique, then add it and loop
///       - otherwise, stop and report R as a clique
///
/// Bron-Kerbosch is like that, but a bit smarter:
///
/// - R is a set of vertices currently known to be in a clique
/// - P is a set of candidate vertices that could be added next
/// - X is a set of ignored vertices that can't be part of the clique
///
/// The algorithm starts with R and X empty, and all the graph vertices in P. Since we begin with
/// zero clique members, we can add any vertex to begin the clique. This vertex moves from P into
/// R, and we prepare to recurse. In order to find a second vertex to add, we need to only consider
/// vertices that are neighbors of the one we've just picked. We take the current P value and
/// filter it to only include immediate neighbors of the starting vertex.
///
/// After one recursion, we have:
///
/// - R is a set of vertices (just our starting vertex)
/// - P is a set of candidate vertices that could be added next (all neighbors of R)
/// - X is empty since we haven't added anything to it yet
///
/// So in a degenerate case (if P is now empty) then we just have a clique of one vertex, and we
/// report that. Otherwise, we iterate through candidates in our new candidate set and find the
/// neighbors of each second vertex. We set the next P value to be the intersection of the current
/// P (neighbors of the first vertex) and the second P (neighbors of the second) since any
/// candidate for a third vertex must be neighbors of both. Like the previous more naive algorithm,
/// this continues until there are no more candidates (new vertices that are neighbors of every
/// existing vertex) at which point we can report a clique.
///
/// Finally, we need to make sure that we don't report the same cliques over and over when starting
/// from different vertices within them. After recursing, we remove the current candidate vertex
/// from P so that it's not included as a candidate in future excursions at that level. Also, the
/// algorithm tracks a third set X which includes the list of candidates that have been processed.
/// We don't add a clique to the result if a vertex in X is a neighbor of the current candidate.
/// This ensures (?) that just because we've removed a processed vertex from P, we don't then just
/// carry on and report the sub-clique which excludes that vertex.
///
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

    // take a copy of P since we mutate it inside the loop
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
