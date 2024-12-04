use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;

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
    let mut xmas_matches = 0;
    for candidate_pos in grid.iter_positions_rc() {
        // eprintln!("checking pos {}", pos);
        'dir: for direction in RCDirection::eight() {
            let mut pos = candidate_pos;
            for target_char in target {
                if grid.index_opt(pos) != Some(target_char) {
                    continue 'dir;
                }

                pos = pos + direction;
            }
            // ran out of target chars, so found a match
            // eprintln!("found match at {} in dir {}", pos, direction);
            xmas_matches += 1;
        }
    }

    let mut x_mas_matches = 0;
    let [dir1, dir2, dir3, dir4] = [
        RCDirection::right() + RCDirection::down(),
        RCDirection::down() + RCDirection::left(),
        RCDirection::left() + RCDirection::up(),
        RCDirection::up() + RCDirection::right(),
    ];
    for candidate_pos in grid.iter_positions_rc() {
        if grid.index_opt(candidate_pos) != Some('A') {
            continue;
        }

        let c1 = grid.index_opt(candidate_pos + dir1);
        let c2 = grid.index_opt(candidate_pos + dir2);
        let c3 = grid.index_opt(candidate_pos + dir3);
        let c4 = grid.index_opt(candidate_pos + dir4);

        let diag1 = (c1 == Some('M') && c3 == Some('S')) || (c1 == Some('S') && c3 == Some('M'));
        let diag2 = (c2 == Some('M') && c4 == Some('S')) || (c2 == Some('S') && c4 == Some('M'));

        if diag1 && diag2 {
            x_mas_matches += 1;
        }
    }

    let part1 = xmas_matches;
    let part2 = x_mas_matches;
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
    assert_eq!(part2, 9);
    Ok(())
}
