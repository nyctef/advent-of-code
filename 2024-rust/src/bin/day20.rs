use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::{hash_map::Entry, VecDeque};

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

fn solve_for(input: &str) -> Result<(usize, usize)> {
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

    let path_positions = path.iter().map(|s| s.pos).collect_vec();

    let mut path_lookup = GridLookup::<usize>::new(grid.width(), grid.height());
    for s in path {
        path_lookup.set(&s.pos, s.ps);
    }

    let part1 = count_cheats(
        grid.width(),
        grid.height(),
        &path_positions,
        &path_lookup,
        2,
    );
    let part2 = count_cheats(
        grid.width(),
        grid.height(),
        &path_positions,
        &path_lookup,
        20,
    );

    Ok((part1, part2))
}

fn count_cheats(
    width: usize,
    height: usize,
    path: &[CharGridIndexRC],
    path_lookup: &GridLookup<usize>,
    allowed_cheat_time: usize,
) -> usize {
    let mut cheats_by_start_end = FxHashMap::default();
    let mut seen = GridLookup::<bool>::new(width, height);
    let mut search = VecDeque::new();
    for path_point in path {
        let time = path_lookup.get(path_point);
        seen.clear();
        search.clear();

        search.push_front((0, *path_point));
        while let Some((cheat_time, next_pos)) = search.pop_front() {
            if seen.get_opt(&next_pos) != Some(&false) {
                continue;
            }
            seen.set(&next_pos, true);

            if cheat_time > allowed_cheat_time {
                continue;
            }

            if next_pos != *path_point {
                if let Some(time_after_cheat) = path_lookup.get_opt(&next_pos) {
                    let time_after_cheat = *time_after_cheat as isize;
                    let current_time = (time + cheat_time) as isize;

                    let time_saved = time_after_cheat - current_time;
                    if time_saved >= 100 {
                        let by_start_end = cheats_by_start_end.entry((path_point, next_pos));

                        if let Entry::Vacant(e) = by_start_end {
                            e.insert(time_saved);
                        }
                    }
                }
            }
            for dir in RCDirection::four() {
                let next_next_pos = next_pos + dir;
                search.push_back((cheat_time + 1, next_next_pos));
            }
        }
    }

    cheats_by_start_end.len()
}

#[derive(Debug)]
struct GridLookup<T> {
    width: usize,
    height: usize,
    cells: Vec<T>,
}

impl<T: Default + Clone> GridLookup<T> {
    fn new(width: usize, height: usize) -> Self {
        GridLookup {
            width,
            height,
            cells: vec![T::default(); width * height],
        }
    }

    fn get_opt(&self, index: &CharGridIndexRC) -> Option<&T> {
        if index.row >= self.height || index.col >= self.width {
            None
        } else {
            Some(&self.cells[index.row * self.width + index.col])
        }
    }

    fn get(&self, index: &CharGridIndexRC) -> &T {
        &self.cells[index.row * self.width + index.col]
    }

    fn set(&mut self, index: &CharGridIndexRC, value: T) {
        if index.row >= self.height || index.col >= self.width {
            return;
        }
        self.cells[index.row * self.width + index.col] = value;
    }

    fn clear(&mut self) {
        // TODO: is there a faster way to zero out a vec like this?
        self.cells.clear();
        self.cells.resize(self.width * self.height, T::default());
    }
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
