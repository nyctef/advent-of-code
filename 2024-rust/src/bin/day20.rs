use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 20)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct State {
    ps: usize,
    pos: CharGridIndexRC,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ps.cmp(&other.ps)
    }
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let mut grid = CharGrid::from_string(input);

    let start = grid.find_single_char('S');
    let end = grid.find_single_char('E');
    grid.set_index_rc(start, '.');
    grid.set_index_rc(end, '.');

    let mut search = Dijkstra::new(|s: &State| s.pos);
    search.push(State { ps: 0, pos: start });

    let get_next_candidates = |s: State| {
        let mut nexts = vec![];

        for dir in RCDirection::four() {
            let target_pos = s.pos + dir;
            if grid.index_opt(target_pos) == Some('.') {
                nexts.push(State {
                    ps: s.ps + 1,
                    pos: target_pos,
                });
            }
        }

        nexts
    };
    let is_target_state = |s: &State| s.pos == end;

    let result = search.run_single(get_next_candidates, is_target_state);

    dbg!(result);

    todo!();
    let part1 = 0;
    let part2 = 0;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 0);
    assert_eq!(part2, 0);
    Ok(())
}
