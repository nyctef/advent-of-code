use std::collections::{hash_map::RandomState, HashMap};

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 8)?;

    let result = solve_part_2(&input)?;

    println!("{}", result);
    Ok(())
}

fn parse_input(input: &str) -> (Vec<char>, HashMap<String, (String, String)>) {
    let (instructions, network) = input.trim().split_once("\n\n").unwrap();
    let instructions = instructions.chars().collect_vec();
    let network = network.lines().map(|l| {
        let (name, nodes) = l.split_once("=").unwrap();
        let name = name.trim();
        let nodes = nodes.replace("(", "").replace(")", "");
        let (left_node, right_node) = nodes.split_once(", ").unwrap();
        (
            name.to_string(),
            (left_node.trim().to_string(), right_node.trim().to_string()),
        )
    });
    let network: HashMap<String, (String, String), RandomState> = HashMap::from_iter(network);
    (instructions, network)
}

fn solve_part_1(input: &str) -> Result<String> {
    let mut step_count = 0;

    let (instructions, network) = parse_input(input);

    dbg!(&instructions, &network);
    let mut current_node_name = "AAA";
    let mut current_node = network.get("AAA").unwrap();
    let mut instructions = instructions.iter().cycle();
    loop {
        if current_node_name == "ZZZ" {
            break;
        }
        step_count += 1;
        let next_instruction = instructions.next().unwrap();
        let next_node_name = match next_instruction {
            'L' => &current_node.0,
            'R' => &current_node.1,
            _ => panic!("Unknown instruction {}", next_instruction),
        };
        println!("Going {} to get {}", next_instruction, next_node_name);
        current_node_name = next_node_name;
        current_node = network.get(next_node_name).unwrap();
    }

    let part1 = step_count;
    Ok(format!("Part 1: {part1}"))
}

fn solve_part_2(input: &str) -> Result<String> {
    let (instructions, network) = parse_input(input);

    let mut current_node_names = vec![];
    let mut current_nodes = vec![];
    for (k, v) in network.iter() {
        if k.ends_with("A") {
            current_node_names.push(k);
            current_nodes.push(v);
        }
    }

    dbg!(&current_node_names, &current_nodes);
    let mut loops = vec![];

    for i in 0..current_node_names.len() {
        let mut step_count: u64 = 0;
        let mut instructions = instructions.iter().cycle();
        let mut current_node_name = current_node_names[i];
        let mut current_node = current_nodes[i];
        loop {
            if current_node_name.ends_with("Z") {
                break;
            }
            step_count += 1;
            let next_instruction = instructions.next().unwrap();
            let next_node_name = match next_instruction {
                'L' => &current_node.0,
                'R' => &current_node.1,
                _ => panic!("Unknown instruction {}", next_instruction),
            };
            // println!("Going {} to get {}", next_instruction, next_node_name);
            current_node_name = next_node_name;
            current_node = network.get(next_node_name).unwrap();
        }
        println!(
            "Starting from {} took {} steps to reach a Z-ending node ({})",
            current_node_names[i], step_count, current_node_name
        );
        loops.push(step_count);
    }

    let result = loops.iter().fold(1, |a, n| num::integer::lcm(a, *n));

    Ok(format!("lcm of each cycle is {}", result))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"###;
    let result = solve_part_1(input)?;

    assert_eq!("Part 1: 2", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"###;
    let result = solve_part_1(input)?;

    assert_eq!("Part 1: 6", result);
    Ok(())
}

#[test]
fn test_example3() -> Result<()> {
    let input = r###"
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"###;
    let result = solve_part_2(input)?;

    assert_eq!("Part 2: 6", result);
    Ok(())
}
