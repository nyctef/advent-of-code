use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::{HashMap, HashSet};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 22)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    // let mut is_supporting: HashMap<usize, Vec<usize>, _> = FxHashMap::default();
    // let mut is_supported_by: HashMap<usize, Vec<usize>, _> = FxHashMap::default();
    // let mut is_landed: HashSet<usize, _> = FxHashSet::default();
    let mut bricks = input
        .trim()
        .lines()
        .map(|l| {
            let digits = all_numbers_usize(l);
            assert_eq!(digits.len(), 6);
            let b = Brick::new(
                Point3::new(digits[0], digits[1], digits[2]),
                Point3::new(digits[3], digits[4], digits[5]),
            );
            // assert the bricks only have length in one dimension (ie they're more like sticks)
            // or they're a single brick long
            assert!(
                (b.start.x != b.end.x)
                    ^ (b.start.y != b.end.y)
                    ^ (b.start.z != b.end.z)
                    ^ (b.start == b.end),
                "checking line {}",
                l
            );

            b
        })
        .collect_vec();

    // sort bricks so they go from bottom to top
    bricks.sort_by_key(|b| b.zmin());
    // usize indexes from now on will refer to the index in this sorted list

    let height = bricks.iter().map(|b| b.zmax()).max().unwrap();

    // if we drop bricks starting from the bottom, we shouldn't ever have to re-drop a brick
    for i in 0..bricks.len() {
        let mut brick = bricks[i];
        'outer: loop {
            let next_brick = brick.down();

            if next_brick.intersects_z_at(0) {
                // hit the ground
                break;
            }

            for i2 in 0..bricks.len() {
                if i2 == i {
                    // don't intersect with self
                    continue;
                }

                // println!("trying to intersect {:?} and {:?}", next_brick, bricks[i2]);
                if next_brick.intersects(bricks[i2]) {
                    // println!("intersected!")
                    // we've tried to move brick down into next_brick,
                    // but we hit bricks[i2]
                    // TODO: record this collision
                    break 'outer;
                }
            }

            brick = next_brick;
        }
        bricks[i] = brick;

    }


    dbg!(&bricks, &height);

    let part1 = "";
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Constructor)]
struct Point3 {
    x: usize,
    y: usize,
    z: usize,
}

impl std::fmt::Debug for Point3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("P3({},{},{})", self.x, self.y, self.z))
    }
}

impl Point3 {
    fn down(&self) -> Point3 {
        Point3::new(self.x, self.y, self.z - 1)
    }

}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy, Constructor)]
struct Brick {
    start: Point3,
    end: Point3,
}
impl Brick {
    fn xmin(&self) -> usize {
        self.start.x.min(self.end.x)
    }
    fn xmax(&self) -> usize {
        self.start.x.max(self.end.x)
    }
    fn ymin(&self) -> usize {
        self.start.y.min(self.end.y)
    }
    fn ymax(&self) -> usize {
        self.start.y.max(self.end.y)
    }
    fn zmin(&self) -> usize {
        self.start.z.min(self.end.z)
    }
    fn zmax(&self) -> usize {
        self.start.z.max(self.end.z)
    }
    fn intersects_z_at(&self, z: usize) -> bool {
        self.zmin() <= z && z <= self.zmax()
    }
    fn down(&self) -> Brick {
        Brick::new(self.start.down(), self.end.down())
    }

    fn intersects(&self, other: Brick) -> bool {
        // try to find a separating axis
        if self.xmax() < other.xmin() || other.xmax() < self.xmin() {
            return false;
        } 
        if self.ymax() < other.ymin() || other.ymax() < self.ymin() {
            return false;
        } 
        if self.zmax() < other.zmin() || other.zmax() < self.zmin() {
            return false;
        } 
        true
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9 
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: ", result);
    Ok(())
}
