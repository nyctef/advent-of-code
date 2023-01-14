use crate::{
    aoc_util::*,
    intcode::{IntCode, MachineState, TInt},
};
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::collections::HashMap;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 17)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct PointRC {
    r: i32,
    c: i32,
}

#[rustfmt::skip]
impl PointRC {
    fn north(&self) -> PointRC { PointRC { r: self.r - 1, c: self.c }}
    fn south(&self) -> PointRC { PointRC { r: self.r + 1, c: self.c }}
    fn east(&self)  -> PointRC { PointRC { r: self.r, c: self.c + 1 }}
    fn west(&self)  -> PointRC { PointRC { r: self.r, c: self.c - 1 }}
}

impl PointRC {
    fn move_in(&self, dir: &Dir) -> PointRC {
        match dir {
            Dir::North => self.north(),
            Dir::South => self.south(),
            Dir::East => self.east(),
            Dir::West => self.west(),
        }
    }
}

#[derive(Clone, Copy)]
enum Dir {
    North,
    South,
    East,
    West,
}

impl Dir {
    fn right(&self) -> Dir {
        match self {
            Dir::North => Dir::East,
            Dir::South => Dir::West,
            Dir::East => Dir::South,
            Dir::West => Dir::North,
        }
    }
    fn left(&self) -> Dir {
        match self {
            Dir::North => Dir::West,
            Dir::South => Dir::East,
            Dir::East => Dir::North,
            Dir::West => Dir::South,
        }
    }
}

fn solve_for(input: &str) -> Result<String> {
    let mut machine = input.parse::<IntCode>()?;
    machine.run()?;
    let mut map = vec![];
    while let Some(c) = machine.read_output() {
        map.push(c as u8 as char);
    }

    let map = map.iter().join("");

    println!("{}", &map);

    let scaffolds = parse_map(&map);

    let mut align_param_sum = 0;
    for (p, _) in &scaffolds {
        // if north, south, east and west are also in the collection, then this is an intersection
        if scaffolds.contains_key(&p.north())
            && scaffolds.contains_key(&p.south())
            && scaffolds.contains_key(&p.east())
            && scaffolds.contains_key(&p.west())
        {
            align_param_sum += p.r * p.c;
        }
    }

    let full_path = find_full_path_for_map(scaffolds, PointRC { r: 12, c: 2 });
    println!("full path: {}", full_path);

    let mut machine_2 = input.parse::<IntCode>()?;
    machine_2.set_value_at_position(0, 2);
    machine_2.run()?;
    assert!(machine_2.state().1 == MachineState::AwaitingInput);

    let program = "C,A,C,B,C,A,B,A,B,A
R,10,R,10,L,4
R,4,L,4,L,10,L,10
R,10,R,10,R,6,R,4
n
";

    for c in program.chars() {
        machine_2.queue_input(c as TInt);
    }

    machine_2.run()?;
    machine_2.run()?;
    machine_2.run()?;
    machine_2.run()?;
    machine_2.run()?;

    assert_eq!(MachineState::Halted, machine_2.state().1);

    let mut dust_collected: TInt = 0;
    while let Some(x) = machine_2.read_output() {
        dust_collected = x;
    }

    Ok(format!(
        "part 1: {} part 2: {}",
        align_param_sum, dust_collected
    ))
}

fn parse_map(map: &str) -> HashMap<PointRC, char> {
    let scaffolds = map
        .trim()
        .lines()
        .enumerate()
        .map(|(r, l)| {
            l.chars().enumerate().map(move |(c, char)| {
                (
                    PointRC {
                        r: r as i32,
                        c: c as i32,
                    },
                    char,
                )
            })
        })
        .flatten()
        .filter(|(_p, c)| c == &'#')
        .collect::<HashMap<_, _>>();
    scaffolds
}

fn find_full_path_for_map(scaffolds: HashMap<PointRC, char>, start: PointRC) -> String {
    // we assume that we start by facing up and need to turn right
    // (since this is true of both the puzzle input and our example)
    let mut steps = vec![];
    let mut facing = Dir::North;
    let mut current = start;

    loop {
        let can_go_left = scaffolds.contains_key(&current.move_in(&facing.left()));
        let can_go_right = scaffolds.contains_key(&current.move_in(&facing.right()));

        if can_go_left {
            steps.push("L".to_owned());
            facing = facing.left();
        } else if can_go_right {
            steps.push("R".to_owned());
            facing = facing.right()
        } else {
            // end of the line
            break;
        }

        let mut fitbit = 0;
        loop {
            let next_step = current.move_in(&facing);
            if !scaffolds.contains_key(&next_step) {
                break;
            } else {
                current = next_step;
                fitbit += 1;
            }
        }
        steps.push(fitbit.to_string());
    }

    steps.join(",")
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......
"###;
    let scaffolds = parse_map(input);
    let result = find_full_path_for_map(scaffolds, PointRC { r: 6, c: 0 });

    assert_eq!(
        "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2",
        result
    );
    Ok(())
}
