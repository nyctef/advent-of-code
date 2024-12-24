use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use rustc_hash::FxHashSet;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 16)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Debug, Constructor, Clone, Eq, PartialEq, Hash)]
struct State {
    score: usize,
    pos: CharGridIndexRC,
    dir: RCDirection,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score.cmp(&other.score)
    }
}

fn solve_for(input: &str) -> Result<(usize, usize)> {
    let mut grid = CharGrid::from_string(input);

    let start = grid.find_single_char('S');
    let end = grid.find_single_char('E');
    grid.set_index_rc(start, '.');
    grid.set_index_rc(end, '.');

    let mut search = Dijkstra::new(|s: &State| (s.pos, s.dir));
    search.push(State::new(0, start, RCDirection::right()));

    let get_next_candidates = |s: State| {
        let mut nexts = vec![];

        if grid[s.pos + s.dir] == '.' {
            nexts.push(State::new(s.score + 1, s.pos + s.dir, s.dir));
        }
        let dir_left = s.dir.counterclockwise();
        if grid[s.pos + dir_left] == '.' {
            nexts.push(State::new(s.score + 1000, s.pos, dir_left));
        }
        let dir_right = s.dir.clockwise();
        if grid[s.pos + dir_right] == '.' {
            nexts.push(State::new(s.score + 1000, s.pos, dir_right));
        }

        nexts
    };

    let is_target_state = |s: &State| s.pos == end;

    let results = search.run_all(get_next_candidates, is_target_state);

    let is_starting_state = |s: &State| s.pos == start;

    let get_prev_candidates = |s: &State| {
        let mut nexts = vec![];
        let char_in_front = grid[s.pos - s.dir];
        if char_in_front == '.' {
            nexts.push(State::new(s.score - 1, s.pos - s.dir, s.dir));
        }

        let dir_left = s.dir.counterclockwise();
        if grid[s.pos - dir_left] == '.' {
            nexts.push(State::new(s.score - 1000, s.pos, dir_left));
        }
        let dir_right = s.dir.clockwise();
        if grid[s.pos - dir_right] == '.' {
            nexts.push(State::new(s.score - 1000, s.pos, dir_right));
        }
        nexts
    };

    let all_paths =
        search.reconstruct_path_all(results[0].clone(), get_prev_candidates, is_starting_state);

    let part2 = FxHashSet::from_iter(all_paths.into_iter().flatten().map(|s| s.pos)).len();

    let part1 = results[0].score;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 7036);
    assert_eq!(part2, 45);
    Ok(())
}
