use std::collections::HashMap;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 15)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let input = input.trim().replace("\n", "");
    let instructions = input.split(",").collect_vec();

    let mut boxes: HashMap<u8, Vec<(String, u32)>> = HashMap::new();

    for instruction in &instructions {
        let instruction = *instruction;

        if instruction.ends_with("-") {
            let label = &instruction[..instruction.len() - 1];

            let box_ = aoc_hash(label);
            let entry = boxes.entry(box_).or_insert(vec![]);

            entry.retain(|(l, _f)| l != label);
        } else if instruction.contains("=") {
            let (label, f) = instruction.split_once('=').unwrap();

            let box_ = aoc_hash(label);
            let entry = boxes.entry(box_).or_insert(vec![]);
            let new_value = (label.to_owned(), f.parse().unwrap());

            if let Some(i) = entry.iter().position(|(lb, _f)| lb == label) {
                entry[i] = new_value;
            } else {
                entry.push(new_value);
            }
        } else {
            panic!("unrecognised instr {}", instruction);
        }
    }

    let mut total: u32 = 0;
    for entry in instructions {
        total += aoc_hash(entry) as u32;
    }

    let mut foc_power: u32 = 0;
    for (box_, lenses) in boxes.iter() {
        for (n, (_, f)) in lenses.iter().enumerate() {
            foc_power += (*box_ as u32 + 1) * (n as u32 + 1) * f;
        }
    }

    Ok(format!("total: {total} foc_power: {foc_power}"))
}

fn aoc_hash(entry: &str) -> u8 {
    let mut hash = 0 as u8;
    for c in entry.chars() {
        hash = hash.wrapping_add(c as u8);
        hash = hash.wrapping_mul(17);
    }
    hash
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
"###;
    let result = solve_for(input)?;

    assert_eq!("total: 1320 foc_power: 145", result);
    Ok(())
}
