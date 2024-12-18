use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 18)?;

    let (part1, part2) = solve_for(&input, 70, 70, 1024)?;

    println!("Part 1: {} | Part 2: {:?}", part1, part2);
    Ok(())
}

#[derive(Constructor, Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Pos {
    x: usize,
    y: usize,
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
        Some(self.cmp(&other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.steps.cmp(&other.steps)
    }
}

fn solve_for(input: &str, width: usize, height: usize, count: usize) -> Result<(usize, Pos)> {
    let bytes = input
        .trim()
        .lines()
        .map(all_numbers_usize)
        .map(|ns| Pos::new(ns[0], ns[1]))
        .collect_vec();

    let mut part1 = 0;
    let mut part2 = Pos::new(0, 0);
    for num_bytes in count..bytes.len() {
        let bytes = bytes.iter().copied().take(num_bytes).collect_vec();

        // eprintln!("adding byte {:?}", bytes[bytes.len() - 1]);

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

            // eprintln!("s {:?} | {:?} ", s, nexts);

            nexts
        };
        let res = search.run_single(get_neighbors, |&s| s.pos.x == width && s.pos.y == height);
        if res.is_none() {
            part2 = bytes[bytes.len() - 1];
            break;
        } else if part1 == 0 {
            part1 = res.unwrap().steps
        }
    }

    Ok((part1, part2))
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
    let (part1, part2) = solve_for(input, 6, 6, 12)?;

    assert_eq!(part1, 22);
    assert_eq!(part2, 0);
    Ok(())
}
