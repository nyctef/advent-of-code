use std::collections::HashMap;

use crate::util::*;
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 6)?;

    let part1 = count_total_orbits(&input);
    let part2 = count_orbit_transfers(&input, "YOU", "SAN");
    println!("part1: {part1} part2: {part2}");

    Ok(())
}

fn parse_input(input: &str) -> HashMap<&str, &str> {
    let mut orbit_parents: HashMap<&str, &str> = HashMap::new();
    for line in input.lines() {
        let split = line.split(')').collect::<Vec<_>>();
        let parent = split[0];
        let child = split[1];

        orbit_parents.insert(child, parent);
    }
    orbit_parents
}

fn count_total_orbits(input: &str) -> usize {
    let orbit_parents = parse_input(input);

    let mut all_children = orbit_parents.keys().collect::<Vec<_>>();
    all_children.sort();

    let mut total: usize = 0;
    for x in all_children {
        let count_for_child = list_parent_orbits(&orbit_parents, x).len();
        println!("{x} orbits {count_for_child} other bodies");
        total += count_for_child;
    }
    total
}

fn list_parent_orbits<'a>(
    orbit_parents: &'a HashMap<&'a str, &'a str>,
    child: &str,
) -> Vec<&'a str> {
    let mut result: Vec<&'a str> = vec![];
    let mut parent = orbit_parents.get(child);
    while let Some(next_parent) = parent {
        result.push(next_parent);
        parent = orbit_parents.get(next_parent);
    }
    result
}

fn count_orbit_transfers(input: &str, start: &str, end: &str) -> usize {
    let orbit_parents = parse_input(input);

    let my_orbit_chain = list_parent_orbits(&orbit_parents, start);
    let target_orbit_chain = list_parent_orbits(&orbit_parents, end);

    let common_prefix = my_orbit_chain
        .iter()
        .rev()
        .zip(target_orbit_chain.iter().rev())
        .take_while(|(x, y)| x == y)
        .count();

    dbg!(&my_orbit_chain, &target_orbit_chain, &common_prefix);

    my_orbit_chain.len() - common_prefix + target_orbit_chain.len() - common_prefix
}

#[test]
fn test1() {
    let example = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
";

    assert_eq!(42, count_total_orbits(example));

    let example2 = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN";

    assert_eq!(4, count_orbit_transfers(example2, "YOU", "SAN"))
}
