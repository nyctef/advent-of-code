use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::{
    collections::{hash_map::Entry, HashMap},
    ops::Not,
};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 7)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for<'input>(input: &'input str) -> Result<String> {
    let components = input
        .trim()
        .lines()
        .map(|l| {
            let (component, output) = l.split_once(" -> ").unwrap();
            let parts = component.split(" ").collect_vec();

            let component = if parts.len() == 1 && parts[0].chars().all(|c| c.is_ascii_digit()) {
                Component::Constant {
                    value: parts[0].parse().unwrap(),
                }
            } else if parts.len() == 1 {
                Component::Wire { in1: parts[0] }
            } else if parts.len() == 2 && parts[0] == "NOT" {
                Component::Not { in1: parts[1] }
            } else if parts.len() == 3 && parts[1] == "AND" {
                Component::And {
                    in1: parts[0],
                    in2: parts[2],
                }
            } else if parts.len() == 3 && parts[1] == "OR" {
                Component::Or {
                    in1: parts[0],
                    in2: parts[2],
                }
            } else if parts.len() == 3 && parts[1] == "LSHIFT" {
                Component::Lshift {
                    in1: parts[0],
                    shift: parts[2].parse().unwrap(),
                }
            } else if parts.len() == 3 && parts[1] == "RSHIFT" {
                Component::Rshift {
                    in1: parts[0],
                    shift: parts[2].parse().unwrap(),
                }
            } else {
                panic!("unknown component type {}", l);
            };

            (output, component)
        })
        .into_group_map();

    let mut resolved = HashMap::default();
    let part1 = resolve(&mut resolved, &components, &"a");

    let mut resolved2 = HashMap::default();
    resolved2.insert("b", part1);
    let part2 = resolve(&mut resolved2, &components, &"a");

    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn resolve<'input>(
    resolved: &mut HashMap<&'input str, u16>,
    components: &HashMap<&'input str, Vec<Component<'input>>>,
    name: &'input str,
) -> u16 {
    if let Some(v) = resolved.get(name) {
        return *v;
    }

    // println!("resolving {}", name);
    if name.chars().all(|c| c.is_ascii_digit()) {
        return name.parse().unwrap();
    }
    let component = &components
        .get(name)
        .unwrap_or_else(|| panic!("failed to resolve component <{}>", name));
    assert!(component.len() == 1, "components: {:?}", component);
    let component = component[0];
    let result = match component {
        Component::Constant { value } => value,
        Component::Wire { in1 } => resolve(resolved, components, in1),
        Component::And { in1, in2 } => {
            resolve(resolved, components, in1) & resolve(resolved, components, in2)
        }
        Component::Or { in1, in2 } => {
            resolve(resolved, components, in1) | resolve(resolved, components, in2)
        }
        Component::Lshift { in1, shift } => resolve(resolved, components, in1) << shift,
        Component::Rshift { in1, shift } => resolve(resolved, components, in1) >> shift,
        Component::Not { in1 } => resolve(resolved, components, in1).not(),
    };

    resolved.insert(name, result);
    result
}

#[derive(Debug, Copy, Clone)]
enum Component<'a> {
    Constant { value: u16 },
    Wire { in1: &'a str },
    And { in1: &'a str, in2: &'a str },
    Or { in1: &'a str, in2: &'a str },
    Lshift { in1: &'a str, shift: u16 },
    Rshift { in1: &'a str, shift: u16 },
    Not { in1: &'a str },
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> a
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 65079 | Part 2: ", result);
    Ok(())
}
