use std::{fmt::Write, ops::BitXor};

use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 10)?;

    let (part1, part2) = solve_for(&input, 256);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str, knot_size: usize) -> (u16, String) {
    let lengths = all_numbers_usize(input.trim());

    let mut knot = Knot::with_size(knot_size);
    let mut skip_size = 0;
    let mut pos = 0;

    #[allow(clippy::explicit_counter_loop)]
    for l in lengths {
        let mut slice = knot.slice(pos, l);
        slice.reverse();

        pos += l;
        pos += skip_size;
        skip_size += 1;
    }

    let part1 = knot.check();

    let input_as_bytes = Vec::from(input.trim());

    let part2 = Knot::run_full(&input_as_bytes);

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

impl Knot {
    fn run_full(input: &[u8]) -> String {
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

        let part2 = knot.dense_hash();
        part2
    }
    fn with_size(size: usize) -> Self {
        Knot {
            items: (0..size).map(|x| x as u8).collect_vec()
        }
    }

    fn check(&self) -> u16 {
        self.items[0] as u16 * self.items[1] as u16
    }

    fn slice(&mut self, start: usize, length: usize) -> RingSliceMut {
        RingSliceMut::new(self, start, length)
    }

    fn dense_hash(self) -> String {
        let batches = self.items.into_iter().chunks(16);

        batches
            .into_iter()
            .fold(String::with_capacity(32), |mut a, n| {
                let x = n.fold(0, BitXor::bitxor);
                a.write_fmt(format_args!("{:02x}", x)).unwrap();
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
    let input = r###"
3, 4, 1, 5
"###;
    let (part1, _) = solve_for(input, 5);

    assert_eq!(part1, 12);
}
