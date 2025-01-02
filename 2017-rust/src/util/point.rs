use derive_more::Constructor;
use std::ops::{Add, AddAssign};

#[derive(Constructor, Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct PointXY {
    x: i64,
    y: i64,
}

impl PointXY {
    pub fn zero() -> Self {
        PointXY::new(0, 0)
    }

    pub fn to(&self, other: PointXY) -> DirXY {
        DirXY::new(other.x - self.x, other.y - self.y)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct DirXY {
    dx: i64,
    dy: i64,
}

static EIGHT: [DirXY; 8] = [
    DirXY::new(1, 1),
    DirXY::new(1, 0),
    DirXY::new(1, -1),
    DirXY::new(0, 1),
    DirXY::new(0, -1),
    DirXY::new(-1, 1),
    DirXY::new(-1, 0),
    DirXY::new(-1, -1),
];

impl DirXY {
    pub const fn new(dx: i64, dy: i64) -> Self {
        DirXY { dx, dy }
    }

    pub fn manhattan(&self) -> u64 {
        self.dx.unsigned_abs() + self.dy.unsigned_abs()
    }

    pub fn eight() -> &'static [DirXY] {
        &EIGHT
    }
}

impl Add<DirXY> for PointXY {
    type Output = PointXY;

    fn add(self, rhs: DirXY) -> Self::Output {
        PointXY {
            x: self.x + rhs.dx,
            y: self.y + rhs.dy,
        }
    }
}

impl AddAssign<DirXY> for PointXY {
    fn add_assign(&mut self, rhs: DirXY) {
        self.x += rhs.dx;
        self.y += rhs.dy;
    }
}

pub struct YDown {}
impl YDown {
    pub fn right() -> DirXY {
        DirXY::new(1, 0)
    }
    pub fn left() -> DirXY {
        DirXY::new(1, 0)
    }
    pub fn up() -> DirXY {
        DirXY::new(0, -1)
    }
    pub fn down() -> DirXY {
        DirXY::new(0, 1)
    }
    pub fn counterclockwise(x: DirXY) -> DirXY {
        DirXY::new(x.dy, -x.dx)
    }
}
