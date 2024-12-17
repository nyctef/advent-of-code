use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;

use serde_json::Value;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 12)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let part1 = input
        .trim()
        .lines()
        .flat_map(|l| all_numbers_isize(l).into_iter())
        .sum::<isize>();

    let part2 = input
        .trim()
        .lines()
        .map(|l| {
            let v: Value = l.parse().unwrap();
            better_dead(&v)
        })
        .sum::<isize>();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn better_dead(v: &Value) -> isize {
    match v {
        Value::Number(n) => n.as_i64().unwrap() as isize,
        Value::String(_) => 0,
        Value::Array(a) => a.iter().map(better_dead).sum(),
        Value::Object(o) => {
            if o.values().any(is_red) {
                0
            } else {
                o.values().map(better_dead).sum()
            }
        }
        Value::Bool(_) => 0,
        Value::Null => 0,
    }
}

fn is_red(v: &Value) -> bool {
    if let Value::String(s) = v {
        s == "red"
    } else {
        false
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
[1,2,3]
[1,{"c":"red","b":2},3]
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 12 | Part 2: 10", result);
    Ok(())
}
