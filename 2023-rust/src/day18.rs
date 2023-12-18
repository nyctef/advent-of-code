use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use regex::Regex;
use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Add, Mul},
    str::FromStr,
};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 18)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let re = Regex::from_str(r"^(.) (\d+) \((.*)\)$")?;
    let lines = input
        .trim()
        .lines()
        .map(|l| re.captures(l).unwrap())
        .map(|c| (c[1].chars().nth(0).unwrap(), c[2].parse::<u32>().unwrap()))
        .collect_vec();

    // dbg!(&lines);

    // let mut grid = XYGrid::new();
    let mut current = XYIndex::new(0, 0);
    // let mut updown = 'o';
    // grid.set(current, 'o');
    let mut updown_ranges = vec![];
    let mut max = XYIndex::new(0, 0);
    let mut min = XYIndex::new(0, 0);

    for (dir_char, count) in lines {
        let dir = match dir_char {
            'U' => XYDirection::up(),
            'L' => XYDirection::left(),
            'R' => XYDirection::right(),
            'D' => XYDirection::down(),
            _ => panic!("unrecognised direction {}", dir_char),
        };

        if dir_char == 'U' || dir_char == 'D' {
            let x = current.x;
            let ystart = current.y;
            let yend = current.y + (dir.ydiff * count as isize);
            let ymin = ystart.min(yend);
            let ymax = ystart.max(yend);
            // dbg!(&current, &dir, ystart, yend, ymin, ymax);
            // println!();
            updown_ranges.push((dir_char, x, ymin, ymax));
        }

        /*
        match dir_char {
            'U' => updown = 'U',
            'D' => updown = 'D',
            _ => {}
        };
        grid.set(current, updown);
        */
        current = current + (dir * count as isize);
        max = max.max(&current);
        min = min.min(&current);
    }

    // dbg!(&updown_ranges, max, min);

    let mut holes: u64 = 0;
    for y in min.y..=max.y {
        println!();
        println!("checking y={}", y);
        let mut matching_ranges = updown_ranges
            .iter()
            .filter(|r| y >= r.2 && y <= r.3)
            .collect_vec();
        matching_ranges.sort_by_key(|r| r.1);
        println!("matching ranges: {:?}", matching_ranges);

        let mut parity = 0;
        let mut updown = ' ';
        let mut x_start = isize::min_value();

        for (ud, x, _ymin, _ymax) in matching_ranges {
            println!("checking {} range at {}", ud, x);
            if *ud != updown {
                println!(" changing from {} to {}", updown, ud);
                updown = *ud;
                parity = 1 - parity;
                if parity == 1 {
                    println!("  starting a line at {}", x);
                    // beginning of line to fill
                    x_start = *x;
                } else {
                    let new_holes = (1 + *x - x_start) as u64;
                    println!("  ending a line at {} ({} new holes)", x, new_holes);
                    // end of line to fill
                    holes += new_holes;
                    x_start = *x;
                }
            } else {
                println!(" no change");
                let new_holes = (*x - x_start) as u64;
                println!("  continuing a line at {} ({} new holes)", x, new_holes);
                holes += new_holes;
                x_start = *x;
            }
        }
    }

    /*
    let mut parity = 0;
    let mut updown = ' ';
    let mut prev_y = 99999;
    for (i, c) in grid.enumerate_chars_xy().collect_vec() {
        if i.y != prev_y {
            prev_y = i.y;
            parity = 0;
            updown = ' ';
        }

        if c == 'U' || c == 'D' {
            if c != updown {
                updown = c;
                parity = 1 - parity;
            }
        } else if c == ' ' {
            if parity == 1 {
                grid.set(i, '#');
            }
        }
    }
    */

    // println!("{}", grid);
    Ok(format!("holes: {holes}"))
}

struct XYGrid {
    points: HashMap<XYIndex, char>,
    min: XYIndex,
    max: XYIndex,
}

impl XYGrid {
    pub fn new() -> XYGrid {
        XYGrid {
            points: HashMap::new(),
            min: XYIndex::new(0, 0),
            max: XYIndex::new(0, 0),
        }
    }

    fn set(&mut self, pos: XYIndex, c: char) {
        self.points.insert(pos, c);
        self.min = self.min.min(&pos);
        self.max = self.max.max(&pos);
    }

    fn enumerate_chars_xy(&self) -> impl Iterator<Item = (XYIndex, char)> + '_ {
        (self.min.y..=self.max.y)
            .rev()
            .flat_map(|y| (self.min.x..=self.max.x).map(move |x| XYIndex::new(x, y)))
            .map(|i| (i, self.points.get(&i).copied().unwrap_or(' ')))
    }

    fn point_count(&self) -> usize {
        self.points.len()
    }
}

impl Display for XYGrid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // note we reverse y since we have to print the top line first
        for y in (self.min.y..=self.max.y).rev() {
            for x in self.min.x..=self.max.x {
                f.write_fmt(format_args!(
                    "{}",
                    self.points.get(&XYIndex::new(x, y)).unwrap_or(&' ')
                ))?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Constructor)]
struct XYIndex {
    x: isize,
    y: isize,
}

impl XYIndex {
    pub fn min(&self, other: &Self) -> Self {
        Self {
            x: self.x.min(other.x),
            y: self.y.min(other.y),
        }
    }
    pub fn max(&self, other: &Self) -> Self {
        Self {
            x: self.x.max(other.x),
            y: self.y.max(other.y),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Constructor)]
struct XYDirection {
    xdiff: isize,
    ydiff: isize,
}

impl XYDirection {
    pub fn up() -> XYDirection {
        XYDirection::new(0, 1)
    }
    pub fn down() -> XYDirection {
        XYDirection::new(0, -1)
    }
    pub fn left() -> XYDirection {
        XYDirection::new(-1, 0)
    }
    pub fn right() -> XYDirection {
        XYDirection::new(1, 0)
    }
}

impl Add<XYDirection> for XYIndex {
    type Output = XYIndex;

    fn add(self, rhs: XYDirection) -> Self::Output {
        XYIndex::new(self.x + rhs.xdiff, self.y + rhs.ydiff)
    }
}
impl Mul<isize> for XYDirection {
    type Output = XYDirection;

    fn mul(self, rhs: isize) -> Self::Output {
        XYDirection::new(self.xdiff * rhs, self.ydiff * rhs)
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"###;
    let result = solve_for(input)?;

    assert_eq!("holes: 62", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    // made up example
    //   0123456
    // 0 ####### -> 7
    // 1 #     # -> 7
    // 2 # ### # -> 7
    // 3 # # # # -> 6
    // 4 ### ### -> 6
    let input = r###"
R 6 (#xxxxx0)
D 4 (#xxxxx0)
L 2 (#xxxxx0)
U 2 (#xxxxx0)
L 2 (#xxxxx0)
D 2 (#xxxxx0)
L 2 (#xxxxx0)
U 4 (#xxxxx0)
"###;
    let result = solve_for(input)?;

    assert_eq!("holes: 33", result);
    Ok(())
}

