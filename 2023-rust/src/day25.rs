use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::collections::HashMap;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 25)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for<'input>(input: &'input str) -> Result<String> {
    let lines = input.trim().lines();
    let components: Vec<(&'input str, Vec<&'input str>)> = lines
        .map(|l| {
            let (name, connections) = l.split_once(':').unwrap();
            let connections = connections
                .trim()
                .split(' ')
                .map(|c| c.trim())
                .collect_vec();
            (name, connections)
        })
        .collect_vec();

    let mut connections: FxHashMap<&'input str, Vec<&'input str>> = Default::default();
    for (n, cs) in components {
        connections.entry(n).or_insert(vec![]).extend(&cs);
        for c in cs {
            connections.entry(c).or_insert(vec![]).push(n);
        }
    }

    // dbg!(&components);

    /*
    println!("\n\n\n");
    println!("digraph aoc {{");
    for (n, cs) in components {
        for c in cs {
            println!("{n} -> {c}");

        }
    }
    println!("}}");
    */

    // let mut edge_connection_counts : HashMap<(&'input str, &'input str)> = ;
    //
    //

    let broken = [("lnf", "jll"), ("cmj", "qhd"), ("vtv", "kkp")];

    let mut count1 = 0;
    let mut search1 = Search::new_bfs();
    search1.push("fsl");
    while let Some(n) = search1.pop() {
        count1 += 1;
        'outer: for c in &connections[n] {
            for b in &broken {
                if (n == b.0 && c == &b.1) || (n == b.1 && c == &b.0) {
                    continue 'outer;
                }
            }
            search1.push(c);
        }
    }
    let mut count2 = 0;
    let mut search2 = Search::new_bfs();
    search2.push("sdn");
    while let Some(n) = search2.pop() {
        count2 += 1;
        'outer: for c in &connections[n] {
            for b in &broken {
                if (n == b.0 && c == &b.1) || (n == b.1 && c == &b.0) {
                    continue 'outer;
                }
            }
            search2.push(c);
        }
    }

    println!("count1 {count1}");
    println!("count2 {count2}");
    Ok(format!("{}", count1 * count2))
}

#[test]
#[ignore]
fn test_example1() -> Result<()> {
    let input = r###"
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: ", result);
    Ok(())
}
