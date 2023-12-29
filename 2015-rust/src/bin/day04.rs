use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;

pub fn main() -> Result<()> {
    let input = get_input(2015, 4)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let input = input.trim();
    let mut buffer = input.to_owned();
    let mut part1 = 0;
    let mut part2 = 0;
    for i in 0.. {
        buffer.truncate(input.len());
        buffer.push_str(&i.to_string());

        if i % 1_000_000 == 0 {
            println!("testing {}", &buffer);
        }

        let digest = md5::compute(&buffer.as_bytes());
        if digest.starts_with(&[0, 0]) && digest[2] < 16 && part1 == 0 {
            part1 = i;
        }
        if digest.starts_with(&[0, 0, 0]) {
            part2 = i;
            break;
        }
    }
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r"abcdef";
    let result = solve_for(input)?;

    assert_eq!("Part 1: 609043 | Part 2: ", result);
    Ok(())
}

#[test]
fn test_example_md5() {
    let input = "abcdef609043".to_string();
    let digest = md5::compute(&input);
    assert!(
        digest.starts_with(&[0, 0]) && digest[2] < 16,
        "digest {:?} should start with five zeros | {:?} | {} {:?}",
        digest,
        digest.0,
        input,
        input.as_bytes()
    );
}
