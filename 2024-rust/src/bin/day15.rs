use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 15)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, usize)> {
    let (map, movements) = input.trim().split_once("\n\n").unwrap();
    let mut grid = CharGrid::from_string(map);
    let movements = movements
        .chars()
        .map(|c| match c {
            '<' => Some(RCDirection::left()),
            '>' => Some(RCDirection::right()),
            '^' => Some(RCDirection::up()),
            'v' => Some(RCDirection::down()),
            _ => None,
        })
        .filter_map(|d| d)
        .collect_vec();

    let mut robot_pos = grid
        .enumerate_chars_rc()
        .filter_map(|(p, c)| if c == '@' { Some(p) } else { None })
        .exactly_one()
        .unwrap();

    for &movement in &movements {
        if try_move(&mut grid, robot_pos, movement) {
            robot_pos = robot_pos + movement;
        }
    }

    let gps = grid
        .enumerate_chars_rc()
        .filter_map(|(p, c)| if c == 'O' { Some(p) } else { None })
        .map(|p| 100 * p.row + p.col)
        .sum();

    let mut map2 = map
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '#' => "##",
                    'O' => "[]",
                    '.' => "..",
                    '@' => "@.",
                    '\n' => "\n",
                    _ => unreachable!(),
                })
                .join("")
        })
        .collect_vec();

    // the top and bottom border will be twice as wide as they should be
    let proper_length = map2[1].len();
    for l in &mut map2 {
        if l.len() > proper_length {
            l.truncate(proper_length);
        }
    }

    let grid2 = CharGrid::from_string(&map2.join("\n"));

    let mut robot_pos = grid
        .enumerate_chars_rc()
        .filter_map(|(p, c)| if c == '@' { Some(p) } else { None })
        .exactly_one()
        .unwrap();

    for movement in movements {
        if could_move(&grid, robot_pos, movement) {
            assert!(try_move(&mut grid, robot_pos, movement));
            robot_pos = robot_pos + movement;
        }
    }
    let gps2 = grid
        .enumerate_chars_rc()
        .filter_map(|(p, c)| if c == 'O' { Some(p) } else { None })
        .map(|p| 100 * p.row + p.col)
        .sum();

    Ok((gps, gps2))
}

fn could_move(grid: &CharGrid, pos: CharGridIndexRC, dir: RCDirection) -> bool {
    let target_pos = pos + dir;
    if !grid.is_in_bounds(target_pos) {
        // should be prevented by `#` borders
        panic!("tried to move off the edge");
    }

    let could_move_one = |p, d| {
    let tp = p + d;
        if grid[p] == '#' {
            false
        } else if grid[p] == '.' || could_move(grid, tp, d) {
            true
        } else {
            false
        }
    };

    if grid[pos] == '[' || grid[pos] == ']' {
        if dir == RCDirection::left() || dir == RCDirection::right() {
            // these behave the same as otherwise
            could_move(grid, target_pos, dir)
        } else {
            // up or down: we need to treat the [ and ] as coupled
            // find the other pair:
            let other_pos = if grid[pos] == '[' {
                pos + RCDirection::right()
            } else {
                pos + RCDirection::left()
            };

            return could_move_one(pos, dir) && could_move_one(other_pos, dir);
        }
    } else {
        return could_move_one(pos, dir);
    }
}

fn try_move(grid: &mut CharGrid, pos: CharGridIndexRC, dir: RCDirection) -> bool {
    let target_pos = pos + dir;
    if !grid.is_in_bounds(target_pos) {
        // should be prevented by `#` borders
        panic!("tried to move off the edge");
    }
    if grid[target_pos] == '#' {
        false
    } else if grid[target_pos] == '.' || try_move(grid, target_pos, dir) {
        grid.set_index_rc(target_pos, grid[pos]);
        grid.set_index_rc(pos, '.');
        true
    } else {
        false
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 10092);
    assert_eq!(part2, 0);
    Ok(())
}
