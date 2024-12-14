use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use rustc_hash::FxHashSet;
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
    let part2;
    let mut seconds = 0;
    loop {
        move_robots(&mut robots, width, height);

        seconds += 1;
        let mut per_pos = FxHashSet::default();
        let mut seen_dupe = false;
        for r in &robots {
            if !per_pos.insert(r.pos) {
                seen_dupe = true;
                break;
            }
        }

        if seconds == 100 {
            part1 = safety_factor(&robots, width, height)
        }

        if !seen_dupe {
            // lucky guess about the conditions for the easter egg:
            // let's assume that every robot is on a unique tile when it happens
            print_state_at_time(seconds, height, width, &robots);
            part2 = seconds;
            break;
        }
    }

    Ok((part1, part2))
}

fn safety_factor(robots: &[Robot], width: isize, height: isize) -> u64 {
    let mut quads = vec![0; 4];

    for robot in robots {
        if robot.pos.x < width / 2 {
            if robot.pos.y < height / 2 {
                quads[0] += 1;
            } else if robot.pos.y > height / 2 {
                quads[1] += 1;
            }
        } else if robot.pos.x > width / 2 {
            if robot.pos.y < height / 2 {
                quads[2] += 1;
            } else if robot.pos.y > height / 2 {
                quads[3] += 1;
            }
        }
    }
    let part1 = quads.into_iter().product();
    part1
}

fn move_robots(robots: &mut Vec<Robot>, width: isize, height: isize) {
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
    let (part1, part2) = solve_for(input, 11, 7)?;

    assert_eq!(part1, 12);
    assert_eq!(part2, 0);
    Ok(())
}
