use std::collections::VecDeque;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use rustc_hash::FxHashSet;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 18)?;

    let part1 = part1(&input, 70, 70, 1024)?;
    let part2 = part2(&input, 70, 70, 1024)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Constructor, Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Pos {
    x: isize,
    y: isize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct State {
    pos: Pos,
    steps: usize,
}

impl State {
    fn next(&self, new_pos: Pos) -> Self {
        State {
            pos: new_pos,
            steps: self.steps + 1,
        }
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.steps.cmp(&other.steps)
    }
}

fn part1(input: &str, width: isize, height: isize, count: usize) -> Result<usize> {
    let bytes = input
        .trim()
        .lines()
        .map(all_numbers_isize)
        .map(|ns| Pos::new(ns[0], ns[1]))
        .collect_vec();

    // just take first n for part 1
    let bytes = bytes.into_iter().take(count).collect_vec();

    let mut search = Dijkstra::new(|s: &State| s.pos);
    search.push(State {
        pos: Pos::new(0, 0),
        steps: 0,
    });
    let get_neighbors = |s: State| {
        let mut nexts = Vec::with_capacity(4);
        if s.pos.x > 0 {
            nexts.push(s.next(Pos::new(s.pos.x - 1, s.pos.y)));
        }
        if s.pos.y > 0 {
            nexts.push(s.next(Pos::new(s.pos.x, s.pos.y - 1)));
        }
        if s.pos.x < width {
            nexts.push(s.next(Pos::new(s.pos.x + 1, s.pos.y)));
        }
        if s.pos.y < height {
            nexts.push(s.next(Pos::new(s.pos.x, s.pos.y + 1)));
        }

        nexts = nexts
            .into_iter()
            .filter(|s| !bytes.contains(&s.pos))
            .collect_vec();

        nexts
    };
    let res = search.run_single(get_neighbors, |&s| s.pos.x == width && s.pos.y == height);

    let part1 = res.unwrap().steps;
    Ok(part1)
}

fn part2(input: &str, width: isize, height: isize, initial_count: usize) -> Result<String> {
    let input_bytes = input
        .trim()
        .lines()
        .map(all_numbers_isize)
        .map(|ns| Pos::new(ns[0], ns[1]))
        .collect_vec();
    let mut bytes = FxHashSet::from_iter(input_bytes.iter().take(initial_count).copied());

    for num_bytes in initial_count + 1..input_bytes.len() {
        let newest_byte = input_bytes[num_bytes - 1];
        assert!(bytes.insert(newest_byte));
        // eprintln!("newest {:?}", newest_byte);

        // flood fill from newest byte to see if we create an unbroken chain spanning the field
        let mut search = VecDeque::new();
        let mut seen = FxHashSet::default();
        search.push_front(newest_byte);
        while let Some(pos @ Pos { x, y }) = search.pop_front() {
            // eprintln!("len {} seen {} pos {:?}", search.len(), seen.len(), pos);
            if !seen.insert(pos) {
                continue;
            }
            if x == 0 || x == width || y == 0 || y == width {
                let is_touching_left_edge = seen.iter().any(|p| p.x == 0);
                let is_touching_top_edge = seen.iter().any(|p| p.y == 0);
                let is_touching_bottom_edge = seen.iter().any(|p| p.y == height);
                let is_touching_right_edge = seen.iter().any(|p| p.x == width);

                // we need something that's approximately going from the bottom left corner to the
                // top right corner
                if (is_touching_bottom_edge || is_touching_left_edge)
                    && (is_touching_top_edge || is_touching_right_edge)
                {
                    return Ok(format!("{},{}", newest_byte.x, newest_byte.y));
                }
            }

            let nexts = vec![
                Pos::new(x + 1, y + 1),
                Pos::new(x, y + 1),
                Pos::new(x - 1, y + 1),
                Pos::new(x - 1, y),
                Pos::new(x - 1, y - 1),
                Pos::new(x, y - 1),
                Pos::new(x + 1, y - 1),
                Pos::new(x + 1, y),
            ];

            for n in nexts {
                if !bytes.contains(&n) {
                    continue;
                } else {
                    search.push_front(n);
                }
            }
        }
    }

    panic!("couldn't find a part 2 solution");
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
"###;
    let part1 = part1(input, 6, 6, 12)?;
    let part2 = part2(input, 6, 6, 12)?;

    assert_eq!(part1, 22);
    assert_eq!(part2, "6,1");
    Ok(())
}
