use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 4)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let grid = CharGrid::from_string(input);
    let target = ['X', 'M', 'A', 'S'];
    let mut matches = 0;
    for candidate_pos in grid.iter_positions_rc() {
        // eprintln!("checking pos {}", pos);
        'dir: for direction in RCDirection::eight() {
            let mut pos = candidate_pos;
            for target_char in target {
                if !grid.is_in_bounds(pos) {
                    continue 'dir;
                }
                if grid.index(pos) != target_char {
                    continue 'dir;
                }
            
                pos = pos + direction;

            }
            // ran out of target chars, so found a match
            // eprintln!("found match at {} in dir {}", pos, direction);
            matches += 1;
        }
    }

    let part1 = matches;
    let part2 = 0;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 18);
    assert_eq!(part2, 0);
    Ok(())
}
