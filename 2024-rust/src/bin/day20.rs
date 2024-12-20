use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

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

    // dbg!(&result);

    let get_prev_candidates = |s: State| {
        let mut prevs = vec![];

        for dir in RCDirection::four() {
            let target_pos = s.pos + dir;
            if grid.index_opt(target_pos) == Some('.') {
                prevs.push(State {
                    ps: s.ps - 1,
                    pos: target_pos,
                });
            }
        }

        prevs
    };
    let is_starting_state = |s: &State| s.pos == start;

    let path =
        search.reconstruct_path_single(result.unwrap(), get_prev_candidates, is_starting_state);

    let path: FxHashMap<_, _> = path.into_iter().map(|s| (s.pos, s.ps)).collect();

    let mut cheats = FxHashMap::default();
    let mut part1 = 0;
    for (path_point, time) in &path {
        for cheat_dir in RCDirection::four() {
            let cheat_target = *path_point + cheat_dir * 2;
            if let Some(&time_after_cheat) = path.get(&cheat_target) {
                let time_after_cheat = time_after_cheat as isize;
                let time = *time as isize;
                let time_saved = time_after_cheat - time - 2;
                if time_saved > 0 {
                    // eprintln!(
                    //     "skipping from {} to {} gains {}ps",
                    //     path_point,
                    //     cheat_target,
                    //     time_saved
                    // );

                    *cheats.entry(time_saved).or_insert(0) += 1;

                    if time_saved >= 100 {
                        part1 += 1;
                    }
                }
            }
        }
    }

    dbg!(&cheats);

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
