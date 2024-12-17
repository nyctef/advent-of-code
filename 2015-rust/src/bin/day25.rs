use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 25)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn get_code_at_n(n: u64) -> u64 {
    let mut ans = 20151125;
    for _ in 2..=n {
        ans *= 252533;
        ans %= 33554393;
    }
    ans
}

fn get_n_for_pos(col: u64, row: u64) -> u64 {
    // triangular numbers
    let n_at_top_of_col = (col * (col + 1)) / 2;
    // dbg!(n_at_top_of_col);

    let mut n = n_at_top_of_col;
    // todo: generalization of triangular numbers starting at n? just calc manually for now
    let incr = col;
    for i in 2..=row {
        n += incr + i - 2;
    }
    n
}

fn solve_for(input: &str) -> Result<u64> {
    let nums = all_numbers_u64(input.trim());
    let row = nums[0];
    let col = nums[1];

    let n = get_n_for_pos(col, row);

    let result = get_code_at_n(n);

    Ok(result)
}

#[test]
fn test1() -> Result<()> {
    assert_eq!(get_code_at_n(2), 31916031);
    assert_eq!(get_code_at_n(3), 18749137);
    Ok(())
}

#[test]
fn test2() -> Result<()> {
    assert_eq!(get_n_for_pos(1, 1), 1);
    assert_eq!(get_n_for_pos(4, 3), 19);
    Ok(())
}
