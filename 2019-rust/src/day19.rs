use std::{collections::HashMap, str::FromStr};

use crate::{intcode::IntCode, util::*};
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 19)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let intcode = IntCode::from_str(input)?;

    let mut total = 0;
    let mut prev_line_total = 0;
    let mut prev_line_start = 0;
    let max_lines = 1_000_000;
    let mut lookup = HashMap::new();
    for y in 0..max_lines {
        if y % 1000 == 0 {
            println!("checking line y={}", y);
        }
        let mut line_total = 0;
        let mut line_start = 0;
        for x in prev_line_start..(prev_line_start + prev_line_total) + 10 {
            let mut p = intcode.clone();
            p.queue_input(x);
            p.queue_input(y);
            p.run()?;
            let result = p.read_output().unwrap();
            total += result;
            line_total += result;
            if result == 1 && line_start == 0 {
                line_start = x;
            }
            // print!("{}", result);
        }
        if line_total != prev_line_total  || line_start != prev_line_start{
            // println!("line {} start {} total {}", y, line_start, line_total);
            prev_line_total = line_total;
            prev_line_start = line_start;
        }
        if line_total >= 100 {
            lookup.insert(y, (line_start, line_total));

            if let Some((earlier_line_start, earlier_line_total)) = lookup.get(&(y - 100)) {
                println!("starting at bottom left corner y={} x={}", y, line_start);
                let required_length = (line_start - earlier_line_start) + 100;
                println!(
                    "comparing required length {} to earlier length {}",
                    required_length, earlier_line_total
                );
                if earlier_line_total >= &required_length {
                    break;
                }
            }
        }
        // println!();
    }

    Ok(format!("total: {}", total))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("expected", result);
    Ok(())
}
