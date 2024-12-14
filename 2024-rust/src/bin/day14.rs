use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::ops::Add;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 14)?;

    let (part1, part2) = solve_for(&input, 101, 103)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Constructor, Debug, Copy, Clone)]
struct Pos {
    x: isize,
    y: isize,
}
#[derive(Constructor, Debug, Copy, Clone)]
struct Vel {
    dx: isize,
    dy: isize,
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

fn solve_for(input: &str, width: isize, height: isize) -> Result<(u64, u64)> {
    let mut robots = input
        .trim()
        .lines()
        .map(all_numbers_isize)
        .map(|n| (Pos::new(n[0], n[1]), Vel::new(n[2], n[3])))
        .collect_vec();

    // dbg!(&robots);

    for i in 0..100 {
        for robot in robots.iter_mut() {
            robot.0 = robot.0 + robot.1;

            if robot.0.x < 0 {
                robot.0.x += width;
            }
            if robot.0.x >= width {
                robot.0.x -= width;
            }
            if robot.0.y < 0 {
                robot.0.y += height;
            }
            if robot.0.y >= height {
                robot.0.y -= height;
            }
        }
    }

    let mut quads = vec![0; 4];

    for robot in &robots {
        if robot.0.x < width/2 {
            if robot.0.y < height / 2 {
                quads[0] += 1;
            } else if robot.0.y > height / 2 {
                quads [1] += 1;
            }
        } else if robot.0.x > height / 2 {
            if robot.0.y < height / 2 {
                quads[2] += 1;
            } else if robot.0.y > height / 2 {
                quads[3] += 1;
            }
        }
    }
    eprintln!("{:?}", robots.iter().map(|r| r.0).collect_vec());
    dbg!(&quads);
    
    let part1 = quads.into_iter().product();

    let part2 = 0;
    Ok((part1, part2))
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

    assert_eq!(part1, 0);
    assert_eq!(part2, 0);
    Ok(())
}
