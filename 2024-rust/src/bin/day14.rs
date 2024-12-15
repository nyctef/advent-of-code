use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use rustc_hash::FxHashSet;
use std::cmp::Ordering::{Greater, Less};
use std::collections::BinaryHeap;
use std::ops::Add;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 14)?;

    let (part1, part2) = solve_for(&input, 101, 103)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Constructor, Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Pos {
    x: isize,
    y: isize,
}
#[derive(Constructor, Debug, Copy, Clone)]
struct Vel {
    dx: isize,
    dy: isize,
}

#[derive(Constructor, Debug, Copy, Clone)]
struct Robot {
    pos: Pos,
    vel: Vel,
}

impl Add<Vel> for Pos {
    type Output = Pos;

    fn add(self, rhs: Vel) -> Self::Output {
        Pos {
            x: self.x + rhs.dx,
            y: self.y + rhs.dy,
        }
    }
}

fn solve_for(input: &str, width: isize, height: isize) -> Result<(u64, i32)> {
    let mut robots = input
        .trim()
        .lines()
        .map(all_numbers_isize)
        .map(|n| Robot::new(Pos::new(n[0], n[1]), Vel::new(n[2], n[3])))
        .collect_vec();

    let mut part1 = 0;
    let mut part2_scores = BinaryHeap::new();
    let mut seconds = 0;
    let max_seconds = width * height; // the robots will probably have looped after this point
    loop {
        move_robots(&mut robots, width, height);

        seconds += 1;

        if seconds == 100 {
            part1 = safety_factor(&robots, width, height)
        }

        part2_scores.push((cohesion_factor(&robots, width, height), seconds));

        if seconds > 100 && seconds > max_seconds as i32 {
            break;
        }
    }

    let (highest_score, part2) = part2_scores.pop().unwrap();

    eprintln!("winner: {}", highest_score);
    for _ in 0..20 {
        eprintln!("runner up: {:?}", part2_scores.pop().unwrap());
    }

    Ok((part1, part2))
}

fn safety_factor(robots: &[Robot], width: isize, height: isize) -> u64 {
    let mut quads = vec![0; 4];

    for robot in robots {
        let (x, y) = (robot.pos.x, robot.pos.y);

        match (x.cmp(&(width / 2)), y.cmp(&(height / 2))) {
            (Less, Less) => quads[0] += 1,
            (Less, Greater) => quads[1] += 1,
            (Greater, Greater) => quads[2] += 1,
            (Greater, Less) => quads[3] += 1,
            // ignore robots on the centerlines
            _ => {}
        }
    }

    quads.into_iter().product()
}

// any image is likely to have groups of robots nearby, whether it's
// a line like
//
// 1...
// .1..
// ..1.
// ...1
//
// or a block like
//
// .112
// ..11
// ...1
//
// we'd expect clusters of robots to happen more often than robots being
// spread out randomly
fn cohesion_factor(robots: &[Robot], _width: isize, _height: isize) -> i32 {
    let mut positions = FxHashSet::default();
    for robot in robots {
        positions.insert(robot.pos);
    }

    let mut total = 0;

    for robot in robots {
        let mut local_score = 0;
        for xoff in -1..=1 {
            for yoff in -1..=1 {
                if (xoff, yoff) == (0, 0) {
                    continue;
                }
                if positions.contains(&Pos::new(robot.pos.x + xoff, robot.pos.y + yoff)) {
                    local_score += 1;
                }
            }
        }

        total += local_score;
    }

    total
}

fn move_robots(robots: &mut [Robot], width: isize, height: isize) {
    for robot in robots.iter_mut() {
        robot.pos = robot.pos + robot.vel;

        if robot.pos.x < 0 {
            robot.pos.x += width;
        }
        if robot.pos.x >= width {
            robot.pos.x -= width;
        }
        if robot.pos.y < 0 {
            robot.pos.y += height;
        }
        if robot.pos.y >= height {
            robot.pos.y -= height;
        }
    }
}

#[allow(dead_code)]
fn print_state_at_time(seconds: i32, height: isize, width: isize, robots: &[Robot]) {
    println!("=======================================");
    println!("    {}     ", seconds);
    println!("=======================================");

    for y in 0..height {
        for x in 0..width {
            let mut count = 0;
            for robot in robots {
                if robot.pos.x == x && robot.pos.y == y {
                    count += 1;
                }
            }
            print!(
                "{}",
                if count == 0 {
                    ' '
                } else {
                    char::from_digit(count, 10).unwrap()
                }
            );
        }
        println!();
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"###;
    let (part1, _) = solve_for(input, 11, 7)?;

    assert_eq!(part1, 12);
    Ok(())
}
