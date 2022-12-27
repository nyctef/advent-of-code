use std::ops;

use crate::aoc_util::*;
use crate::err_util::*;
use regex::Regex;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 3)?;

    let wires = input.lines().map(parse_wire).collect::<Result<Vec<_>>>()?;
    dbg!(wires);

    Ok(())
}

#[derive(Clone, Debug)]
struct Point2 {
    x: i32,
    y: i32,
}

impl Point2 {
    pub fn up() -> Point2 {
        Point2 { x: 0, y: 1 }
    }
    pub fn down() -> Point2 {
        Point2 { x: 0, y: -1 }
    }
    pub fn left() -> Point2 {
        Point2 { x: -1, y: 0 }
    }
    pub fn right() -> Point2 {
        Point2 { x: 1, y: 0 }
    }
}

impl ops::Add<&Point2> for &Point2 {
    type Output = Point2;

    fn add(self, rhs: &Point2) -> Self::Output {
        Point2 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl ops::Mul<i32> for &Point2 {
    type Output = Point2;

    fn mul(self, rhs: i32) -> Self::Output {
        Point2 {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl ops::Mul<i32> for Point2 {
    type Output = Point2;

    fn mul(self, rhs: i32) -> Self::Output {
        Point2 {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

#[derive(Debug)]
struct Line {
    start: Point2,
    end: Point2,
}

fn parse_wire(s: &str) -> Result<Vec<Line>> {
    let parts = s.split(",");
    let mut result = vec![];

    let part_re = Regex::new(r"([ULRD])(\d+)").unwrap();
    let mut p1 = Point2 { x: 0, y: 0 };
    for part in parts {
        let captures = part_re.captures(part).ok_or("failed to parse line part")?;
        let direction = &captures[1];
        let distance: i32 = (&captures[2]).parse()?;

        let mut p2 = match direction {
            "U" => Point2::up(),
            "R" => Point2::right(),
            "D" => Point2::down(),
            "L" => Point2::left(),
            other => panic!("unexpected direction {other} - should have been caught by regex"),
        };

        p2 = p2 * distance;

        result.push(Line {
            start: p1,
            end: p2.clone(),
        });
        p1 = p2;
    }

    Ok(result)
}

#[test]
fn test1() {
    // ...
}
