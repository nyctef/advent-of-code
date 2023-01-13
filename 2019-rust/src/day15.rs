use std::{
    collections::{HashSet, VecDeque},
    fmt::Debug,
};

use crate::{
    aoc_util::*,
    intcode::{IntCode, TInt},
};
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 15)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
struct PointXY(i32, i32);

impl Debug for PointXY {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl PointXY {
    fn surrounding4(&self) -> [(TInt, PointXY); 4] {
        [
            (1, PointXY(self.0, self.1 + 1)), // north
            (2, PointXY(self.0, self.1 - 1)), // south
            (3, PointXY(self.0 - 1, self.1)), // west
            (4, PointXY(self.0 + 1, self.1)), // east
        ]
    }

    fn move_in(&self, dir: TInt) -> PointXY {
        match dir {
            1 => PointXY(self.0, self.1 + 1),
            2 => PointXY(self.0, self.1 - 1),
            3 => PointXY(self.0 - 1, self.1),
            4 => PointXY(self.0 + 1, self.1),
            _ => panic!(),
        }
    }
}

fn solve_for(input: &str) -> Result<String> {
    let mut robot = input.parse::<IntCode>()?;

    // backtrace: the current set of steps needed to return to (0,0)
    let mut backtrace = VecDeque::<TInt>::new();

    let mut current_position = PointXY(0, 0);

    let mut walls = HashSet::<PointXY>::new();
    let mut visited_spots = HashSet::<PointXY>::new();
    let mut macguffin = PointXY(99, 99);

    'outer: loop {
        visited_spots.insert(current_position);
        let mut have_moved_this_turn = false;

        for (dir, candidate) in current_position.surrounding4() {
            if walls.contains(&candidate) || visited_spots.contains(&candidate) {
                //
            } else {
                have_moved_this_turn = true;
                // we've not tried this way yet, so let's give it a go
                let result = try_move(&mut robot, dir);
                if result == 0 {
                    // hit a wall, so don't update position
                    println!("Hit a wall at {candidate:?}");
                    walls.insert(candidate);
                    continue;
                } else {
                    // the robot is now in the new position
                    println!("Moved to {candidate:?}");
                    if result == 2 {
                        println!("!!! Found the thingy at {candidate:?}");
                        macguffin = candidate;
                    }
                    current_position = candidate;
                    backtrace.push_back(opposite_dir(dir));
                    break;
                }
            }
        }
        if !have_moved_this_turn {
            // TODO: we need to only backtrack if all four candidates are invalid

            // we've run out of places to explore from this position
            let backwards = backtrace.pop_back();
            if let Some(backwards_dir) = backwards {
                let result = try_move(&mut robot, backwards_dir);
                assert_ne!(
                    0, result,
                    "the robot should not have hit a wall in the backtrace"
                );
                println!("Backtracking from {:?}", &current_position);
                current_position = current_position.move_in(backwards_dir);
                continue;
            } else {
                // we've run out of backwards steps! I think we're done!
                println!("Ran out of steps!");
                break 'outer;
            }
        }
    }

    let min_x = walls.iter().min_by_key(|w| w.0).unwrap().0;
    let max_x = walls.iter().max_by_key(|w| w.0).unwrap().0;
    let min_y = walls.iter().min_by_key(|w| w.1).unwrap().1;
    let max_y = walls.iter().max_by_key(|w| w.1).unwrap().1;

    dbg!(min_x, max_x, min_y, max_y);

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            if (x, y) == (0, 0) {
                print!("S")
            } else if PointXY(x, y) == macguffin {
                print!("O");
            } else if walls.contains(&PointXY(x, y)) {
                print!("#");
            } else {
                print!(" ")
            }
        }
        println!()
    }

    Ok("todo".to_owned())
}

fn try_move(robot: &mut IntCode, direction: TInt) -> TInt {
    robot.queue_input(direction);
    robot.run().unwrap();
    robot.read_output().unwrap()
}

fn opposite_dir(dir: TInt) -> TInt {
    match dir {
        1 => 2,
        2 => 1,
        3 => 4,
        4 => 3,
        _ => panic!(),
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("expected", result);
    Ok(())
}
