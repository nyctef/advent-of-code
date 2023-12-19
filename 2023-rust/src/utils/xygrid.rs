use std::{collections::HashMap, fmt::Display, ops::{Add, Mul}};
use derive_more::Constructor;


pub struct XYGrid {
    points: HashMap<XYIndex, char>,
    min: XYIndex,
    max: XYIndex,
}

#[allow(dead_code)]
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
pub struct XYIndex {
    pub x: isize,
    pub y: isize,
}

#[allow(dead_code)]
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
pub struct XYDirection {
    pub xdiff: isize,
    pub ydiff: isize,
}

#[allow(dead_code)]
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
