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

    let mut robot_pos = find_robot(&grid);

    // for &movement in &movements {
    //     if do_move(&mut grid, robot_pos, movement) {
    //         robot_pos = robot_pos + movement;
    //     }
    // }

    let gps = grid
        .enumerate_chars_rc()
        .filter_map(|(p, c)| if c == 'O' { Some(p) } else { None })
        .map(|p| 100 * p.row + p.col)
        .sum();

    let mut map = map
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
    let proper_length = map[1].len();
    for l in &mut map {
        if l.len() > proper_length {
            l.truncate(proper_length);
        }
    }

    let mut grid = CharGrid::from_string(&map.join("\n"));

    let mut robot_pos = find_robot(&grid);

    for movement in movements {
        dbg!(&grid);
        eprintln!("move: {}", movement);
        if could_move(&grid, robot_pos, movement) {
            assert!(do_move(&mut grid, robot_pos, movement));
            eprintln!("moving");
            robot_pos = robot_pos + movement;
        }
        debug_assert_eq!(find_robot(&grid), robot_pos);
    }
    let gps2 = grid
        .enumerate_chars_rc()
        .filter_map(|(p, c)| if c == '[' { Some(p) } else { None })
        .map(|p| 100 * p.row + p.col)
        .sum();

    dbg!(&grid);

    Ok((gps, gps2))
}

fn find_robot(grid: &CharGrid) -> CharGridIndexRC {
    grid.enumerate_chars_rc()
        .filter_map(|(p, c)| if c == '@' { Some(p) } else { None })
        .exactly_one()
        .unwrap()
}

fn could_move(grid: &CharGrid, pos: CharGridIndexRC, dir: RCDirection) -> bool {
    let could_move_one = |p, d| {
        let tp = p + d;
        if grid[tp] == '#' {
            false
        } else if grid[tp] == '[' || grid[tp] == ']' {
            could_move(grid, tp, d)
        } else if grid[tp] == '.' {
            true
        } else {
            false
        }
    };

    let target_pos = pos + dir;
    eprintln!("pos {} dir {} target {}", pos, dir, target_pos);
    if !grid.is_in_bounds(target_pos) {
        // should be prevented by `#` borders
        panic!("tried to move off the edge at {} {}", pos, dir);
    }

    if grid[target_pos] == '#' {
        return false;
    }
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

            eprintln!(
                "box {} : checking both pos {} and other_pos {}",
                grid[pos], pos, other_pos
            );

            return could_move_one(pos, dir) && could_move_one(other_pos, dir);
        }
    } else {
        return could_move_one(pos, dir);
    }
}

fn do_move_one(g: &mut CharGrid, p: CharGridIndexRC, d: RCDirection) -> bool {
    let tp = p + d;
    eprintln!(
        "[tm] p {} d {} tp {} g[p] {} g[tp] {}",
        p, d, tp, g[p], g[tp]
    );
    if g[tp] == '#' {
        panic!("tried to move into '#' when it shouldn't be possible");
    } else if g[tp] == '[' || g[tp] == ']' {
        do_move(g, tp, d);
    } else if g[tp] == '.' {
        // continue
    } else {
        panic!("didn't have empty space to move into");
    };
    eprintln!("[tm] y");
    eprintln!("[tm] moving {} at {} to {}", g[p], p, tp);
    g.set_index_rc(tp, g[p]);
    g.set_index_rc(p, '.');
    true
}

fn do_move(grid: &mut CharGrid, pos: CharGridIndexRC, dir: RCDirection) -> bool {
    let target_pos = pos + dir;
    eprintln!(
        "[tm] pos {} dir {} target {} to_move {}",
        pos, dir, target_pos, grid[pos]
    );
    if !grid.is_in_bounds(target_pos) {
        // should be prevented by `#` borders
        panic!("tried to move off the edge at {} {}", pos, dir);
    }

    if grid[target_pos] == '#' {
        panic!("tried to move into '#' when it shouldn't be possible");
    }

    if grid[pos] == '[' || grid[pos] == ']' {
        if dir == RCDirection::left() || dir == RCDirection::right() {
            // these behave the same as otherwise
            do_move(grid, target_pos, dir);
            assert!(do_move_one(grid, pos, dir));
        } else {
            // up or down: we need to treat the [ and ] as coupled
            // find the other pair:
            let other_pos = if grid[pos] == '[' {
                pos + RCDirection::right()
            } else {
                pos + RCDirection::left()
            };

            eprintln!(
                "[tm] box {} : checking both pos {} and other_pos {}",
                grid[pos], pos, other_pos
            );

            assert!(do_move_one(grid, pos, dir));
            assert!(do_move_one(grid, other_pos, dir));
        }
    } else {
        eprintln!("[tm] moving normally {} {}", pos, grid[pos]);
        assert!(do_move_one(grid, pos, dir));
    }

    // eprintln!("[tm] moving {} at {} to {}", grid[pos], pos, target_pos);
    // grid.set_index_rc(target_pos, grid[pos]);
    // grid.set_index_rc(pos, '.');

    true
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
    assert_eq!(part2, 9021);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    let input = r###"
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 10092);
    assert_eq!(part2, 9021);
    Ok(())
}
