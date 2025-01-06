use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::ops::BitXor;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 14)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u32, u64) {
    let mut part1 = 0;
    let mut part2 = 0;
    let input = input.trim();

    for i in 0..128 {
        let hash_input = format!("{input}-{i}");
        let hash_input = Vec::from(hash_input);
        let hash = Knot::run_full(&hash_input);
        part1 += hash.count_ones();
    }

    (part1, part2)
}

#[derive(Debug)]
struct Knot {
    items: Vec<u8>,
}

impl Default for Knot {
    fn default() -> Self {
        Knot::with_size(256)
    }
}

#[allow(dead_code)]
impl Knot {
    fn run_full(input: &[u8]) -> u128 {
        let mut knot = Knot::default();
        let mut skip_size = 0;
        let mut pos = 0;
        let mut input = Vec::from(input);
        input.extend_from_slice(&[17, 31, 73, 47, 23]);

        for _round in 0..64 {
            for &l in &input {
                let mut slice = knot.slice(pos, l.into());
                slice.reverse();

                pos += l as usize;
                pos += skip_size;
                skip_size += 1;
            }
        }

        knot.dense_hash()
    }
    fn with_size(size: usize) -> Self {
        Knot {
            items: (0..size).map(|x| x as u8).collect_vec(),
        }
    }

    fn check(&self) -> u16 {
        self.items[0] as u16 * self.items[1] as u16
    }

    fn slice(&mut self, start: usize, length: usize) -> RingSliceMut {
        RingSliceMut::new(self, start, length)
    }

    fn dense_hash(self) -> u128 {
        let batches = self.items.into_iter().chunks(16);

        batches.into_iter().fold(0_u128, |mut a, n| {
            let x = n.fold(0, BitXor::bitxor);
            a <<= 8;
            a |= Into::<u128>::into(x);
            a
        })
    }
}

#[derive(Debug, Constructor)]
struct RingSliceMut<'a> {
    knot: &'a mut Knot,
    start: usize,
    length: usize,
}

impl RingSliceMut<'_> {
    fn reverse(&mut self) {
        for i in 0..self.length / 2 {
            let s = self.map_index(i);
            let e = self.map_index(self.length - 1 - i);
            let items = &mut self.knot.items;

            (items[s], items[e]) = (items[e], items[s]);
        }
    }

    fn map_index(&self, i: usize) -> usize {
        assert!(i < self.length);
        (self.start + i) % self.knot.items.len()
    }
}

#[test]
fn test_example1() {
    let input = "flqrgnkx";
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 8108);
    assert_eq!(part2, 0);
}
