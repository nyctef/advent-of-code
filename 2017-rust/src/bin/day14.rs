use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::{collections::VecDeque, ops::BitXor};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 14)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u32, u64) {
    let input = input.trim();

    let mut field = (0..128)
        .map(|i| {
            let hash_input = Vec::from(format!("{input}-{i}"));
            Knot::run_full(&hash_input)
        })
        .collect_vec();

    let part1 = field.iter().map(|l| l.count_ones()).sum();

    let mut part2 = 0;

    while let Some((i, line)) = field.iter().find_position(|&&l| l != 0) {
        let col = line.trailing_zeros();
        let pos = (i, col);

        let mut search = VecDeque::new();
        // eprintln!("starting at {pos:?}");
        search.push_front(pos);
        while let Some((r, c)) = search.pop_back() {
            // r: which line (u128) in `field` we're looking at
            // c: which bit in that u128 we're inspecting, counting from the right (LSB)
            //    so   0b0000....00000X is where c=0
            //    and  0b1000....000000 is where c=127
            // eprintln!("now at {:?}", (r, c));
            if field[r] & (1 << c) == 0 {
                // we probably added this cell to the queue via two different paths (eg up-right
                // and right-up)
                continue;
            }
            // toggle this bit off
            field[r] ^= 1 << c;

            if r > 0 && field[r - 1] & (1 << c) > 0 {
                search.push_back((r - 1, c));
            }
            if r < field.len() - 1 && field[r + 1] & (1 << c) > 0 {
                search.push_back((r + 1, c));
            }
            if c > 0 && field[r] & (1 << (c - 1)) > 0 {
                search.push_back((r, c - 1));
            }
            if c < 127 && field[r] & (1 << (c + 1)) > 0 {
                search.push_back((r, c + 1));
            }
        }
        part2 += 1;
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
    assert_eq!(part2, 1242);
}
