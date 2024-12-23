use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashSet;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 23)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (usize, u64) {
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
                    eprintln!("found triple {:?}", triple);
                    triples.insert(triple);
                }
            }
        }
    }

    eprintln!("triples {:?}", triples);

    let mut part1 = triples.len();
    let mut part2 = 0;

    (part1, part2)
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
    assert_eq!(part2, 0);
}
