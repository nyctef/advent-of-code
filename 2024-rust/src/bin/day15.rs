use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

const DIAG_ENABLED: bool = false;
const VIZ_ENABLED: bool = true;

macro_rules! diag {
    ($($arg:tt)*) => {{
        if DIAG_ENABLED {
            eprintln!($($arg)*);
        }
    }};
}

macro_rules! viz {
    ($($arg:tt)*) => {{
        if VIZ_ENABLED {
            eprintln!($($arg)*);
        }
    }};
}

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
        .filter_map(|c| match c {
            '<' => Some(RCDirection::left()),
            '>' => Some(RCDirection::right()),
            '^' => Some(RCDirection::up()),
            'v' => Some(RCDirection::down()),
            _ => None,
        })
        .collect_vec();

    let robot_pos = find_robot(&grid);
    simulate_robot(&movements, &mut grid, robot_pos);
    viz!("{:?}", &grid);
    let gps = calc_gps(&grid);

    let map = expand_map(map);

    let mut grid = CharGrid::from_string(&map.join("\n"));
    let robot_pos = find_robot(&grid);
    simulate_robot(&movements, &mut grid, robot_pos);
    viz!("{:?}", &grid);
    let gps2 = calc_gps(&grid);

    Ok((gps, gps2))
}

fn expand_map(map: &str) -> Vec<String> {
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
    map
}

fn simulate_robot(movements: &[RCDirection], grid: &mut CharGrid, mut robot_pos: CharGridIndexRC) {
    for &movement in movements {
        diag!("{:?}", &grid);
        diag!("move: {}", dir_str(movement));
        if do_move(grid, robot_pos, movement, false) {
            do_move(grid, robot_pos, movement, true);
            diag!("moving");
            robot_pos = robot_pos + movement;
        }
        debug_assert_eq!(find_robot(grid), robot_pos);
    }
}

fn calc_gps(grid: &CharGrid) -> usize {
    grid.filter_chars_rc(|c| c == 'O' || c == '[')
        .map(|p| 100 * p.row + p.col)
        .sum()
}

fn dir_str(dir: RCDirection) -> &'static str {
    if dir == RCDirection::up() {
        "UP"
    } else if dir == RCDirection::down() {
        "DOWN"
    } else if dir == RCDirection::left() {
        "LEFT"
    } else if dir == RCDirection::right() {
        "RIGHT"
    } else {
        panic!("")
    }
}

fn find_robot(grid: &CharGrid) -> CharGridIndexRC {
    grid.find_single_char('@')
}

fn do_move_one(g: &mut CharGrid, p: CharGridIndexRC, d: RCDirection, commit: bool) -> bool {
    let tp = p + d;
    diag!(
        "[tm] p {} d {} tp {} g[p] {} g[tp] {}",
        p,
        d,
        tp,
        g[p],
        g[tp]
    );
    if g[p] == '.' {
        // TODO: this feels like a hack - we've probably got some other logic wrong elsewhere
        // but we can always move empty space
        return true;
    }
    let can_move;
    if g[tp] == '#' {
        return false;
    } else if g[tp] == '[' || g[tp] == ']' || g[tp] == 'O' {
        can_move = do_move(g, tp, d, commit);
    } else if g[tp] == '.' {
        can_move = true;
    } else {
        return false;
    }
    if can_move && commit {
        // since part 2 can create situations like:
        //
        //         #
        //      [][]
        //       []
        //        @
        //
        // in this case if we try to move the robot up and
        // naively recurse through all the boxes that need to move,
        // we'd end up moving the top left box even though the top
        // right box is blocking the whole thing.
        //
        // As a result we do a pass with commit=false to check
        // whether the entire movement is possible, then actually
        // mutate the grid with commit=true

        diag!("[tm] y");
        diag!("[tm] moving {} at {} to {}", g[p], p, tp);
        g.set_index_rc(tp, g[p]);
        g.set_index_rc(p, '.');
    }
    can_move
}

fn do_move(grid: &mut CharGrid, pos: CharGridIndexRC, dir: RCDirection, commit: bool) -> bool {
    if grid[pos] == '.' {
        // TODO: this feels like a hack - we've probably got some other logic wrong elsewhere
        // but we can move empty space
        return true;
    }
    let target_pos = pos + dir;
    diag!(
        "[tm] pos {} dir {} target {} to_move {}",
        pos,
        dir,
        target_pos,
        grid[pos]
    );
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
            do_move(grid, target_pos, dir, commit) && do_move_one(grid, pos, dir, commit)
        } else {
            // up or down: we need to treat the [ and ] as coupled
            // find the other pair:
            let other_pos = if grid[pos] == '[' {
                pos + RCDirection::right()
            } else {
                pos + RCDirection::left()
            };

            diag!(
                "[tm] box {} : checking both pos {} and other_pos {}",
                grid[pos],
                pos,
                other_pos
            );

            do_move_one(grid, pos, dir, commit) && do_move_one(grid, other_pos, dir, commit)
        }
    } else {
        diag!("[tm] moving normally {} {}", pos, grid[pos]);
        do_move_one(grid, pos, dir, commit)
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

    assert_eq!(part1, 908);
    assert_eq!(part2, 618);
    Ok(())
}

#[test]
fn test_custom3() -> Result<()> {
    let input = r###"
#######
#@O.O.#
#.....#
#######

>>>>>
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 209);
    assert_eq!(part2, 218);
    Ok(())
}
