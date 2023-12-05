#![allow(dead_code)]

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::fmt::Debug;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 5)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct Range {
    start: u64,
    len: u64,
}

impl Range {
    fn contains(&self, val: u64) -> bool {
        val >= self.start && val < self.end()
    }

    /// (exclusive)
    fn end(&self) -> u64 {
        self.start + self.len
    }

    fn overlap(&self, other: &Range) -> Option<Range> {
        let start = self.start.max(other.start);
        let end = self.end().min(other.end());
        if start >= end {
            return None;
        }
        Some(Range {
            start: start,
            len: end - start,
        })
    }

    /// what remains of `self` if `other` is taken away
    fn remainder(&self, other: &Range) -> Vec<Range> {
        let overlap = self.overlap(other);
        if overlap.is_none() {
            // no overlap, so self remains intact
            return vec![*self];
        }
        let overlap = overlap.unwrap();
        let mut result = vec![];
        if self.start < overlap.start {
            result.push(Range {
                start: self.start,
                len: overlap.start - self.start,
            });
        }
        if self.end() > overlap.end() {
            result.push(Range {
                start: overlap.end(),
                len: self.end() - overlap.end(),
            });
        }

        result
    }
}

impl Debug for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<{},{}>", self.start, self.len))
    }
}

#[derive(Debug)]
struct Mapping {
    source: Range,
    dest: Range,
}

impl Mapping {
    fn is_in_source(&self, val: u64) -> bool {
        self.source.contains(val)
    }

    fn map(&self, val: u64) -> u64 {
        (val - self.source.start) + self.dest.start
    }

    fn remap(&self, val: Range) -> Vec<Range> {
        let overlap = self.source.overlap(&val);
        if overlap.is_none() {
            return vec![val];
        }
        let overlap = overlap.unwrap();
        let mapped_overlap = Range {
            start: self.map(overlap.start),
            len: overlap.len,
        };

        return vec![mapped_overlap];
    }
}

#[derive(Debug)]
struct Map {
    name: String,
    mappings: Vec<Mapping>,
}

impl Map {
    fn map(&self, val: u64) -> u64 {
        for mapping in &self.mappings {
            if mapping.is_in_source(val) {
                return mapping.map(val);
            }
        }
        return val;
    }
}

fn parse_map(input: &str) -> Map {
    let (name_line, map_lines) = input.split_once("\n").unwrap();
    let mappings = map_lines
        .trim()
        .split("\n")
        .map(|l| {
            let nums = all_numbers_u64(l);
            Mapping {
                source: Range {
                    start: nums[1],
                    len: nums[2],
                },
                dest: Range {
                    start: nums[0],
                    len: nums[2],
                },
            }
        })
        .collect_vec();
    Map {
        name: name_line.trim().to_owned(),
        mappings,
    }
}

fn solve_for(input: &str) -> Result<String> {
    let input = input.trim();
    let (seedsline, mapslines) = input.split_once("\n\n").unwrap();
    let (_, seedsline) = seedsline.split_once(" ").unwrap();
    let seed_ranges = all_numbers_u64(seedsline)
        // .iter()
        .chunks(2)
        .into_iter()
        .map(|x| Range {
            start: x[0],
            len: x[1],
        })
        .collect_vec();

    dbg!(&seed_ranges);

    let maps = mapslines
        .trim()
        .split("\n\n")
        .map(|ll| parse_map(ll))
        .collect_vec();

    // dbg!(&seed_ranges, &maps);

    // let mut locations = Vec::new();
    let mut min_location: u64 = u64::MAX;
    for seed_range in &seed_ranges {
        for seed in seed_range.start..seed_range.end() {
            let mut seed_val = seed as u64;
            for map in &maps {
                seed_val = map.map(seed_val as u64);
            }
            min_location = min_location.min(seed_val);
        }
    }

    Ok(format!("Part 1: | Part 2: {min_location}"))
}

#[test]
fn when_no_intersection() {
    let first = Range { start: 10, len: 5 };
    let second = Range { start: 20, len: 5 };
    let overlap = first.overlap(&second);
    let remaining = first.remainder(&second);
    assert_eq!(overlap, None);
    assert_eq!(remaining, vec![first]);
}

#[test]
fn when_first_is_contained_inside_second() {
    let first = Range { start: 10, len: 5 };
    let second = Range { start: 5, len: 25 };
    let overlap = first.overlap(&second);
    let remaining = first.remainder(&second);
    assert_eq!(overlap, Some(first));
    assert_eq!(remaining, vec![]);
}

#[test]
fn when_second_is_contained_inside_first() {
    let first = Range { start: 5, len: 20 };
    let second = Range { start: 10, len: 5 };
    let overlap = first.overlap(&second);
    let remaining = first.remainder(&second);
    assert_eq!(overlap, Some(second));
    assert_eq!(
        remaining,
        vec![Range { start: 5, len: 5 }, Range { start: 15, len: 10 }]
    );
}

#[test]
fn when_first_and_second_dont_intersect() {
    let first = Range { start: 5, len: 5 };
    let second = Range { start: 10, len: 5 };
    let overlap = first.overlap(&second);
    let remaining = first.remainder(&second);
    assert_eq!(overlap, None);
    assert_eq!(remaining, vec![first]);
}

#[test]
fn range_overlap_contained() {
    let result = Range { start: 0, len: 30 }.overlap(&Range { start: 10, len: 5 });
    assert_eq!(result, Some(Range { start: 10, len: 5 }))
}

#[test]
fn range_overlap_left() {
    let result = Range { start: 5, len: 10 }.overlap(&Range { start: 1, len: 5 });
    assert_eq!(result, Some(Range { start: 5, len: 1 }))
}

#[test]
fn test_remap_range_no_overlap() {
    let mapping = Mapping {
        source: Range { start: 10, len: 5 },
        dest: Range { start: 20, len: 5 },
    };
    let input_range = Range { start: 5, len: 5 };

    assert_eq!(mapping.remap(input_range), vec![input_range]);
}

#[test]
fn test_remap_left_overlap() {
    let mapping = Mapping {
        source: Range { start: 10, len: 5 },
        dest: Range { start: 20, len: 5 },
    };
    let input_range = Range { start: 12, len: 5 };

    assert_eq!(
        mapping.remap(input_range),
        vec![Range { start: 20, len: 3 }, Range { start: 15, len: 2 }]
    );
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: 46", result);
    Ok(())
}
