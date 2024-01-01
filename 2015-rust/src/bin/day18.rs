use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 18)?;

    let result = solve_for(&input, 100)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, time: usize) -> Result<String> {
    let mut grid = CharGrid::from_string(input);

    for _ in 0..time {
        step_grid(&mut grid);
    }

    let part1 = grid
        .enumerate_chars_rc()
        .filter(|&(_p, c)| c == '#')
        .count();

    let mut grid = CharGrid::from_string(input);
    for _ in 0..time {
        let x = grid.width() - 1;
        let y = grid.height() - 1;
        grid.set_index_rc(CharGridIndexRC::new(0, 0), '#');
        grid.set_index_rc(CharGridIndexRC::new(y, 0), '#');
        grid.set_index_rc(CharGridIndexRC::new(0, x), '#');
        grid.set_index_rc(CharGridIndexRC::new(y, x), '#');
        step_grid(&mut grid);
    }
    let x = grid.width() - 1;
    let y = grid.height() - 1;
    grid.set_index_rc(CharGridIndexRC::new(0, 0), '#');
    grid.set_index_rc(CharGridIndexRC::new(y, 0), '#');
    grid.set_index_rc(CharGridIndexRC::new(0, x), '#');
    grid.set_index_rc(CharGridIndexRC::new(y, x), '#');
    let part2 = grid
        .enumerate_chars_rc()
        .filter(|&(_p, c)| c == '#')
        .count();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn step_grid(grid: &mut CharGrid) {
    let mut next_grid = grid.clone();
    for p in next_grid.iter_positions_rc().collect_vec() {
        let c = grid[p];
        let n_count = grid
            .enumerate_8_neighbors(p)
            .filter(|&(_, c)| c == '#')
            .count();

        next_grid.set_index_rc(
            p,
            if (c == '#' && n_count == 2) || n_count == 3 {
                '#'
            } else {
                '.'
            },
        );
    }
    *grid = next_grid;

    // println!("");
    // println!("{:?}", grid);
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
.#.#.#
...##.
#....#
..#...
#.#..#
####..
"###;
    let result = solve_for(input, 5)?;

    assert_eq!("Part 1: 4 | Part 2: 17", result);
    Ok(())
}
