use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use rand::seq::SliceRandom;
use rand::thread_rng;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::{HashMap, VecDeque};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 22)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut is_supporting: HashMap<usize, Vec<usize>, _> = FxHashMap::default();
    let mut is_supported_by: HashMap<usize, Vec<usize>, _> = FxHashMap::default();
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

    // try to make sure we're not making any assumptions based on the input order in the test
    bricks.shuffle(&mut thread_rng());

    // sort bricks so they go from bottom to top
    bricks.sort_by_key(|b| b.zmin());
    // usize indexes from now on will refer to the index in this sorted list

    let _height = bricks.iter().map(|b| b.zmax()).max().unwrap();

    // if we drop bricks starting from the bottom, we shouldn't ever have to re-drop a brick
    for i in 0..bricks.len() {
        let mut brick = bricks[i];
        'outer: loop {
            let next_brick = brick.down();

            if next_brick.intersects_z_at(0) {
                // hit the ground
                break;
            }

            let mut has_hit_something = false;
            for (i2, &brick2) in bricks.iter().enumerate() {
                if i2 == i {
                    // don't intersect with self
                    continue;
                }

                // println!("trying to intersect {:?} and {:?}", next_brick, bricks[i2]);
                if next_brick.intersects(brick2) {
                    // println!("intersected!")
                    // we've tried to move brick down into next_brick,
                    // but we hit bricks[i2]

                    // record this collision
                    is_supporting.entry(i2).or_default().push(i);
                    is_supported_by.entry(i).or_default().push(i2);
                    // need to make sure we record any other collisions as well
                    has_hit_something = true;
                }
            }
            if has_hit_something {
                break 'outer;
            }

            brick = next_brick;
        }
        bricks[i] = brick;
    }

    // dbg!(&bricks, &height, &is_supporting, &is_supported_by);

    let mut removable_bricks: usize = 0;

    for i in 0..bricks.len() {
        let supported_bricks = is_supporting.entry(i).or_default();
        if supported_bricks
            .iter()
            .all(|sb| is_supported_by.entry(*sb).or_default().len() >= 2)
        {
            removable_bricks += 1;
        }
    }

    /*
    println!("is_supported_by graph:");
    println!();
    println!("digraph {{");
    for i in 0..bricks.len() {
        for i2 in is_supported_by.entry(i).or_default() {
            println!("b{} -> b{}", i, i2);
        }
    }
    println!("}}");
    println!();
    println!();
    */

    let mut chain_reaction_total: usize = 0;

    for (i, &brick) in bricks.iter().enumerate() {
        let mut q = VecDeque::new();
        let mut exploded_bricks = FxHashSet::default();
        q.push_front(i);

        while let Some(n) = q.pop_front() {
            if exploded_bricks.contains(&n) {
                continue;
            }

            if (n == i)
                || is_supported_by
                    .entry(n)
                    .or_default()
                    .iter()
                    .all(|s| exploded_bricks.contains(s))
            {
                exploded_bricks.insert(n);
                for sb in is_supporting.entry(n).or_default() {
                    q.push_back(*sb);
                }
            }
        }

        let subtotal = exploded_bricks.len() - 1; // don't count the original brick

        println!(
            "disintegrating brick {} ({:?}) would cause {} other bricks to fall",
            i, brick, subtotal
        );
        chain_reaction_total += subtotal;
    }

    Ok(format!(
        "Part 1: {removable_bricks} | Part 2: {chain_reaction_total}"
    ))
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

    assert_eq!("Part 1: 5 | Part 2: 7", result);
    Ok(())
}
