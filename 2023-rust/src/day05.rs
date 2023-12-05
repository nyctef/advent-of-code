use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 5)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(Debug)]
struct Mapping {
    source_start: u64,
    dest_start: u64,
    length: u64,
}

impl Mapping {
    fn is_in_source(&self, val: u64) -> bool {
        val >= self.source_start && val <= self.source_start + self.length
    }

    fn map(&self, val: u64) -> u64 {
        (val - self.source_start) + self.dest_start
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
            let nums = all_numbers(l);
            Mapping {
                dest_start: nums[0] as u64,
                source_start: nums[1] as u64,
                length: nums[2] as u64,
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
    let seeds = all_numbers(seedsline);

    let maps = mapslines
        .trim()
        .split("\n\n")
        .map(|ll| parse_map(ll))
        .collect_vec();

    // dbg!(&seeds, &maps);

    let mut locations = Vec::new();
    for &seed in &seeds {
        let mut seed_val = seed as u64;
        for map in &maps {
            seed_val = map.map(seed_val as u64);
        }
        println!("{}", seed_val);
        locations.push(seed_val);
    }

    let part1 = locations.iter().min().unwrap();
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
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

    assert_eq!("Part 1: 35 | Part 2: ", result);
    Ok(())
}
