use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;

use crate::aoc_util::*;
use crate::err_util::*;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 10)?;

    let asteroids = parse_map(&input);
    let station = get_most_visible_asteroids(&asteroids);
    dbg!(&station);

    let asteroids_in_order = calculate_asteroid_destruction_order(&asteroids, station);
    let asteroid_200 = asteroids_in_order.get(199);
    dbg!(asteroid_200);

    Ok(())
}

fn calculate_asteroid_destruction_order<'asteroid_list>(
    asteroids: &'asteroid_list [PointRC],
    station: &'asteroid_list PointRC,
) -> Vec<&'asteroid_list PointRC> {
    let mut asteroids_by_angle = asteroids
        .iter()
        .map(|a| (station.slope_to(a), a))
        .into_group_map()
        .into_iter()
        .collect::<Vec<_>>();
    asteroids_by_angle.sort_by_key(|aba| aba.0);
    for (_, asteroid_list) in asteroids_by_angle.iter_mut() {
        asteroid_list.sort_by(|a, b| {
            a.edist_to(station)
                .partial_cmp(&b.edist_to(station))
                .unwrap()
        })
    }

    (0..10)
        .flat_map(|d| asteroids_by_angle.iter().map(move |aba| aba.1.get(d)))
        .flatten()
        .copied() // .copied() here turns a &&PointRC into a &PointRC
        .collect::<Vec<_>>()
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
    // println!(
    //     "candidate at {} sees asteroids with {}/{} distinct slopes",
    //     candidate,
    //     distinct_slopes.len(),
    //     asteroids.len() - 1
    // );
    distinct_slopes.len()
}

#[derive(Debug, PartialEq, Clone)]
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
            // dr should be positive when we're below other (higher row minus lower row)
            dr: self.r - other.r,
            // dc should be positive when other is to the right of us (higher column minus lower column)
            dc: other.c - self.c,
        }
    }

    fn edist_to(&self, other: &PointRC) -> f64 {
        let dr = (self.r - other.r) as f64;
        let dc = (self.c - other.c) as f64;
        (dr * dr + dc * dc).sqrt()
    }
}
impl Display for &PointRC {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r={} c={}", self.r, self.c)
    }
}

#[derive(Debug, Clone, Copy)]
struct Slope {
    dr: i64,
    dc: i64,
}
impl Slope {
    fn atan2_from_north(&self) -> f64 {
        let mut at2 = f64::atan2(self.dr as f64, self.dc as f64);
        // atan2 by itself gives us the anticlockwise rotation from the direction of the positive x axis
        // but we want the *clockwise* rotation from the positive *y* axis.
        // first flip anticlockwise to clockwise:
        at2 = -at2;
        // then rotate backwards: any value at the positive x axis now gets treated as being rotated 90deg
        // from the positive y axis, and those values would have been zero before, so that implies we need
        // to add a quarter turn:
        at2 += std::f64::consts::FRAC_PI_2;
        // we want values left of the y axis to be positive
        if at2 < 0_f64 {
            at2 += std::f64::consts::TAU;
        }
        at2
    }
}
impl PartialEq for Slope {
    fn eq(&self, other: &Self) -> bool {
        self.atan2_from_north() == other.atan2_from_north()
    }
}
impl Eq for Slope {
    // lies, lies, sweet little lies
}
impl Hash for Slope {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.atan2_from_north().to_bits().hash(state);
    }
}
impl Display for Slope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "dr={} dc={}", self.dr, self.dc)
    }
}
impl PartialOrd for Slope {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.atan2_from_north()
            .partial_cmp(&other.atan2_from_north())
    }
}
impl Ord for Slope {
    // more lies
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.atan2_from_north()
            .partial_cmp(&other.atan2_from_north())
            .unwrap()
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

#[test]
fn large_example() {
    let input = ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##";

    let asteroids = parse_map(input);
    let station = get_most_visible_asteroids(&asteroids);
    dbg!(&station);

    let asteroids_in_order = calculate_asteroid_destruction_order(&asteroids, station);
    let asteroid_200 = asteroids_in_order.get(199);

    assert_eq!(Some(&&PointRC { r: 2, c: 8 }), asteroid_200)
}
