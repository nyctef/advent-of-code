use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    let input = get_input(2015, 5)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let part1 = input
        .trim()
        .lines()
        .filter(|l| {
            let mut vowel_count = 0;
            let mut has_double = false;
            let mut has_bad_pair = false;

            let mut last_char = '\0';
            for c in l.chars() {
                if c == last_char {
                    has_double = true;
                }
                if matches!(c, 'a' | 'e' | 'i' | 'o' | 'u') {
                    vowel_count += 1;
                }
                if matches!(
                    (last_char, c),
                    ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y')
                ) {
                    has_bad_pair = true;
                    break;
                }
                last_char = c;
            }
            !has_bad_pair && has_double && vowel_count >= 3
        })
        .count();

    let part2 = input
        .trim()
        .lines()
        .filter(|l| {
            let has_xyx = l
                .chars()
                .tuple_windows::<(_, _, _)>().find(|t| matches!(t, (a, _b, c) if a == c))
                .is_some();

            let has_repeated_pair = l
                .chars()
                // list all pairs of chars
                .tuple_windows::<(_, _)>()
                // attach a position to each pair
                .enumerate()
                // group by the char value of the pair
                .into_group_map_by(|t| t.1)
                .into_iter().find(|e| e.1.iter().tuple_windows().any(|(a, b)| b.0 - a.0 > 1))
                .is_some();
            has_xyx && has_repeated_pair
        })
        .count();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
ugknbfddgicrmopn
aaa
jchzalrnumimnmhp
haegwjzuvuyypxyu
dvszwmarrgswjxmb
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 2 | Part 2: 0", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
qjhvhtzxzqqjkmpb
xxyxx
uurcxstgmygtbstg
ieodomkazucvgmuy
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 0 | Part 2: 2", result);
    Ok(())
}
