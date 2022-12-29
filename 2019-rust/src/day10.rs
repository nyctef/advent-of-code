use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use crate::aoc_util::*;
use crate::err_util::*;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 10)?;

    let asteroids = parse_map(&input);
    let _station = get_most_visible_asteroids(&asteroids);

    Ok(())
}

fn get_most_visible_asteroids(asteroids: &Vec<PointRC>) -> &PointRC {
    asteroids
        .iter()
        .max_by_key(|a| count_distinct_slopes(asteroids, a))
        .unwrap()
}

fn parse_map(input: &str) -> Vec<PointRC> {
    input
        .lines()
        .enumerate()
        .flat_map(|(r, line)| {
            line.chars()
                .enumerate()
                .filter(|(_, c)| *c == '#')
                .map(move |(c, _)| PointRC::new(r, c))
        })
        .collect::<Vec<_>>()
}

fn count_distinct_slopes(asteroids: &Vec<PointRC>, candidate: &PointRC) -> usize {
    let mut distinct_slopes = HashSet::new();
    for other in asteroids {
        if other == candidate {
            // println!("ignoring {} since it's us", other);
            continue;
        }
        distinct_slopes.insert(candidate.slope_to(other));
        // println!(
        //     "testing {} with diff {}\tand slope {}\t: {}",
        //     other,
        //     &(other - candidate),
        //     &(other - candidate).slope(),
        //     is_new
        // );
    }
    println!(
        "candidate at {} sees asteroids with {}/{} distinct slopes",
        candidate,
        distinct_slopes.len(),
        asteroids.len() - 1
    );
    distinct_slopes.len()
}

#[derive(Debug, PartialEq)]
struct PointRC {
    r: i64,
    c: i64,
}
impl PointRC {
    fn new(r: usize, c: usize) -> PointRC {
        PointRC {
            r: r.try_into().unwrap(),
            c: c.try_into().unwrap(),
        }
    }

    fn slope_to(&self, other: &PointRC) -> Slope {
        Slope {
            dr: self.r - other.r,
            dc: self.c - other.c,
        }
    }
}
impl Display for &PointRC {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r={} c={}", self.r, self.c)
    }
}

#[derive(Debug)]
struct Slope {
    dr: i64,
    dc: i64,
}
impl Slope {
    fn as_f64(&self) -> f64 {
        self.dr as f64 / self.dc as f64
    }
}
impl PartialEq for Slope {
    fn eq(&self, other: &Self) -> bool {
        if self.dr.signum() != other.dr.signum() {
            return false;
        }
        if self.dc.signum() != other.dc.signum() {
            return false;
        }

        self.as_f64() == other.as_f64()
    }
}
impl Eq for Slope {
    // lies, lies, sweet little lies
}
impl Hash for Slope {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_f64().to_bits().hash(state);
    }
}
impl Display for Slope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "dr={} dc={}", self.dr, self.dc)
    }
}

#[test]
fn example1() {
    let input = ".#..#
.....
#####
....#
...##";
    let asteroids = parse_map(input);

    assert_eq!(
        PointRC { r: 4, c: 3 },
        *get_most_visible_asteroids(&asteroids)
    );
}

#[test]
fn example2() {
    let input = "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####";
    let asteroids = parse_map(input);

    assert_eq!(
        PointRC { r: 8, c: 5 },
        *get_most_visible_asteroids(&asteroids)
    );
}

#[test]
fn example2b() {
    let input = "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####";

    let map = parse_map(input);
    let best_count = count_distinct_slopes(&map, &PointRC { r: 8, c: 5 });
    assert_eq!(33, best_count);
}
