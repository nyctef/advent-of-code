use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 14)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut grid = CharGrid::from_string(input);

    let rollers = grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == 'O')
        .collect_vec();
    dbg!(&grid);

    for (p, _) in rollers {
        grid.set_index_rc(p, '.');
        let mut current_pos = p;
        loop {
            let next_pos = current_pos.up();
            if next_pos.is_none() {
                break;
            }
            if grid.index(next_pos.unwrap()) != '.' {
                break;
            }
            current_pos = next_pos.unwrap();
        }
        grid.set_index_rc(current_pos, 'O');
    }

    dbg!(&grid);

    let mut part1 = 0;

    for (final_pos, _) in grid
        .enumerate_chars_rc()
        .filter(|(_p, c)| *c == 'O')
        .collect_vec()
    {
        part1 += grid.height() - final_pos.row;
    }

    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 136 | Part 2: ", result);
    Ok(())
}
