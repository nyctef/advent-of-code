use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 9)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (usize, u64) {
    let mut part1 = 0;
    let mut part2 = 0;

    let mut stack = vec![];
    let chars = input.trim().chars();
    for char in chars {
        // eprintln!("stack: {:?} char: {}", stack, char);
        match char {
            _ if stack.last() == Some(&'!') => {
                stack.pop();
            }
            '>' => {
                stack.pop();
            }
            '!' => stack.push('!'),
            _ if stack.last() == Some(&'<') => {
                part2 += 1;
            }
            '{' => stack.push('{'),
            '<' => stack.push('<'),
            '}' => {
                assert!(stack.last() == Some(&'{'));
                part1 += stack.len();
                stack.pop();
            }
            ',' => {}
            _ => panic!("unrecognised char {}", char),
        }
    }

    (part1, part2)
}

#[test]
fn test_example1() {
    let input = r###"
{{<a!>},{<a!>},{<a!>},{<ab>}}
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 3);
    assert_eq!(part2, 0);
}
