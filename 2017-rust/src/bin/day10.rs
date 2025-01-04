use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 10)?;

    let (part1, part2) = solve_for(&input, 255);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str, max: u8) -> (u16, u64) {
    let mut part1 = 0;
    let mut part2 = 0;

    let mut nums = (0..=max).collect_vec();
    let lengths = all_numbers_usize(input.trim());

    let mut knot = Knot::new(nums);
    let mut skip_size = 0;
    let mut pos = 0;

    for l in lengths {
        let mut slice = knot.slice(pos, l.into());
        slice.reverse();

        pos += l as usize;
        pos += skip_size;
        skip_size += 1;
    }

    part1 = knot.check();

    (part1, part2)
}

#[derive(Debug, Constructor)]
struct Knot {
    items: Vec<u8>,
}

impl Knot {
    fn check(&self) -> u16 {
        self.items[0] as u16 * self.items[1] as u16
    }

    fn slice(&mut self, start: usize, length: usize) -> RingSliceMut {
        RingSliceMut::new(self, start, length)
    }
}

#[derive(Debug, Constructor)]
struct RingSliceMut<'a> {
    knot: &'a mut Knot,
    start: usize,
    length: usize,
}

impl<'a> RingSliceMut<'a> {
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
    let (part1, part2) = solve_for(input, 4);

    assert_eq!(part1, 12);
    assert_eq!(part2, 0);
}
