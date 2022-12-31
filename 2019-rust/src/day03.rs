use std::cmp::max;
use std::cmp::min;
use std::ops;

use crate::aoc_util::*;
use color_eyre::eyre::Result;
use color_eyre::Report;
use regex::Regex;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 3)?;

    let wires = input.lines().map(parse_wire).collect::<Result<Vec<_>>>()?;
    let wire1 = &wires[0];
    let wire2 = &wires[1];
    let mut intersection_distances = vec![];
    for l1 in wire1 {
        for l2 in wire2 {
            if let Some(intersection) = l1.intersects(l2) {
                let dist = intersection.mdist(&Point2::origin());
                if dist != 0 {
                    // println!("found intersection with dist {dist} at {intersection:?} when comparing\n  {l1:?} and\n  {l2:?}");
                    intersection_distances.push(dist);
                }
            }
        }
    }
    println!("{:?}", intersection_distances.iter().min());

    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    pub fn origin() -> Point2 {
        Point2 { x: 0, y: 0 }
    }

    pub fn mdist(&self, other: &Point2) -> u32 {
        ((self.x - other.x).abs() + (self.y - other.y).abs()) as u32
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

impl Line {
    fn intersects(&self, other: &Line) -> Option<Point2> {
        if max(self.start.x, self.end.x) < min(other.start.x, other.end.x) {
            // separable on the x axis
            return None;
        }
        if min(self.start.x, self.end.x) > max(other.start.x, other.end.x) {
            // separable on the x axis
            return None;
        }
        if max(self.start.y, self.end.y) < min(other.start.y, other.end.y) {
            // separable on the y axis
            return None;
        }
        if min(self.start.y, self.end.y) > max(other.start.y, other.end.y) {
            // separable on the y axis
            return None;
        }
        // since we happen to know the lines are flat (horizontal or vertical)
        // we know that they intersect if their bounding boxes intersect
        let x = if self.start.x == self.end.x {
            self.start.x
        } else {
            other.start.x
        };
        let y = if self.start.y == self.end.y {
            self.start.y
        } else {
            other.start.y
        };
        Some(Point2 { x, y })
    }
}

fn parse_wire(s: &str) -> Result<Vec<Line>> {
    let parts = s.split(',');
    let mut result = vec![];

    let part_re = Regex::new(r"([ULRD])(\d+)").unwrap();
    let mut p1 = Point2 { x: 0, y: 0 };
    for part in parts {
        let captures = part_re
            .captures(part)
            .ok_or(Report::msg("failed to parse line part"))?;
        let direction = &captures[1];
        let distance: i32 = captures[2].parse()?;

        let mut p2 = match direction {
            "U" => Point2::up(),
            "R" => Point2::right(),
            "D" => Point2::down(),
            "L" => Point2::left(),
            other => panic!("unexpected direction {other} - should have been caught by regex"),
        };

        p2 = &p1 + &(p2 * distance);

        // println!("Adding line segment {direction} {distance} p1={p1:?} p2={p2:?}");
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
    let l1 = Line {
        start: Point2 { x: 0, y: 5 },
        end: Point2 { x: 10, y: 5 },
    };
    let l2 = Line {
        start: Point2 { x: 3, y: 0 },
        end: Point2 { x: 3, y: 10 },
    };
    assert_eq!(Some(Point2 { x: 3, y: 5 }), l1.intersects(&l2));
    assert_eq!(Some(Point2 { x: 3, y: 5 }), l2.intersects(&l1));

    let l3 = Line {
        start: Point2 { x: 3, y: 20 },
        end: Point2 { x: 3, y: 30 },
    };

    assert_eq!(None, l3.intersects(&l1))
}
