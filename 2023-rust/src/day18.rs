use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use regex::Regex;
use std::{collections::HashMap, fmt::Display, ops::Add, str::FromStr};

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

    let mut grid = XYGrid::new();
    let mut current = XYIndex::new(0, 0);
    let mut updown = 'o';
    grid.set(current, 'o');

    for (dir_char, count) in lines {
        let dir = match dir_char {
            'U' => XYDirection::up(),
            'L' => XYDirection::left(),
            'R' => XYDirection::right(),
            'D' => XYDirection::down(),
            _ => panic!("unrecognised direction {}", dir_char),
        };
        match dir_char {
            'U' => updown = 'U',
            'D' => updown = 'D',
            _ => {}
        };
        grid.set(current, updown);
        for i in 0..count {
            current = current + dir;
            grid.set(current, if i < (count - 1) { dir_char } else { updown });
        }
    }

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

    println!("{}", grid);
    let part1 = grid.point_count();
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
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

    fn point_count(&self) -> usize { self.points.len() }
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

    assert_eq!("Part 1: 62 | Part 2: ", result);
    Ok(())
}
