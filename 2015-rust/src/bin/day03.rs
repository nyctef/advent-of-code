use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use rustc_hash::FxHashSet;
use std::{collections::HashSet, hash::BuildHasher};

pub fn main() -> Result<()> {
    let input = get_input(2015, 3)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut part1_points = FxHashSet::default();
    let mut current = XYIndex::new(0, 0);
    part1_points.insert(current);
    for c in input.trim().chars() {
        current = direction(c, current);
        part1_points.insert(current);
    }

    let part1 = part1_points.len();

    // print_points(&part1_points);

    let mut part2_points = FxHashSet::default();
    let mut robo_current = XYIndex::new(0, 0);
    let mut santa_current = XYIndex::new(0, 0);
    for (i, c) in input.trim().chars().enumerate() {
        if i % 2 == 0 {
            santa_current = direction(c, santa_current);
            part2_points.insert(santa_current);
        } else {
            robo_current = direction(c, robo_current);
            part2_points.insert(robo_current);
        }
    }
    // print_points(&part2_points);
    let part2 = part2_points.len();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[allow(dead_code)]
fn print_points(points: &HashSet<XYIndex, impl BuildHasher>) {
    if points.is_empty() {
        println!("<empty>");
        return;
    }
    let minx = points.iter().min_by_key(|p| p.x).unwrap().x;
    let miny = points.iter().min_by_key(|p| p.y).unwrap().y;
    let maxx = points.iter().max_by_key(|p| p.x).unwrap().x;
    let maxy = points.iter().max_by_key(|p| p.y).unwrap().y;

    for y in (miny..maxy).rev() {
        for x in minx..maxx {
            print!(
                "{}",
                if points.contains(&XYIndex::new(x, y)) {
                    '#'
                } else {
                    '.'
                }
            );
        }
        println!();
    }
}

fn direction(c: char, p: XYIndex) -> XYIndex {
    match c {
        '^' => p.up(),
        '>' => p.right(),
        'v' => p.down(),
        '<' => p.left(),
        _ => panic!("unrecognised char {c}"),
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Constructor)]
struct XYIndex {
    x: isize,
    y: isize,
}
impl XYIndex {
    fn up(&self) -> XYIndex {
        XYIndex::new(self.x, self.y + 1)
    }
    fn down(&self) -> XYIndex {
        XYIndex::new(self.x, self.y - 1)
    }
    fn right(&self) -> XYIndex {
        XYIndex::new(self.x + 1, self.y)
    }
    fn left(&self) -> XYIndex {
        XYIndex::new(self.x - 1, self.y)
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
^>v<
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 4 | Part 2: 3", result);
    Ok(())
}
