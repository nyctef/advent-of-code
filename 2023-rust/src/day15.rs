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
    let entries = input.split(",").collect_vec();

    dbg!(&entries);

    let mut total: u32 = 0;
    for entry in entries {
        total += HASH(entry);
    }
    Ok(format!("total: {total}"))
}

fn HASH(entry: &str) -> u32 {
    let mut hash = 0 as u32;
    for c in entry.chars() {
        hash += c as u32;
        hash *= 17;
        hash = hash % 256;
    }
    hash 
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
"###;
    let result = solve_for(input)?;

    assert_eq!("total: 1320", result);
    Ok(())
}
