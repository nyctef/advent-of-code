use std::collections::HashSet;

use crate::aoc_util::*;
use crate::err_util::*;
use crate::intcode::IntCode;
use crate::intcode::MachineState;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 11)?;

    let mut robot = input.parse::<IntCode>()?;
    robot.run()?;
    assert!(robot.state().1 == MachineState::AwaitingInput);

    let mut white_squares: HashSet<PointXY> = HashSet::new();
    let mut squares_painted: HashSet<PointXY> = HashSet::new();
    let mut current_square = PointXY(0, 0);
    let directions = vec![(0, 1), (1, 0), (0, -1), (-1, 0)];
    let mut current_direction = 0_isize;

    while robot.state().1 != MachineState::Halted {
        robot.queue_input(i64::from(white_squares.contains(&current_square)));

        robot.run()?;
        // dbg!(robot.state());

        squares_painted.insert(current_square);

        if let Some(color_to_paint) = robot.read_output() {
            match color_to_paint {
                0 => white_squares.remove(&current_square),
                _ => white_squares.insert(current_square),
            };
        }

        if let Some(direction_to_turn) = robot.read_output() {
            match direction_to_turn {
                0 => {
                    current_direction =
                        (current_direction - 1).rem_euclid(directions.len() as isize)
                }
                _ => {
                    current_direction =
                        (current_direction + 1).rem_euclid(directions.len() as isize)
                }
            }
        }

        let dir = directions[current_direction as usize];
        current_square = PointXY(current_square.0 + dir.0, current_square.1 + dir.1);
    }

    println!("{}", squares_painted.len());

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PointXY(i32, i32);

#[test]
fn test1() {
    // ...
}
