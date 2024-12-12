use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use rustc_hash::FxHashSet;
use std::collections::VecDeque;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 12)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, usize)> {
    let grid = CharGrid::from_string(input);

    let mut seen = FxHashSet::default();
    let mut part1 = 0;
    let mut part2 = 0;
    for (pos, plant) in grid.enumerate_chars_rc() {
        if seen.contains(&pos) {
            continue;
        }

        let mut flood_fill_queue = VecDeque::new();
        flood_fill_queue.push_back(pos);
        let mut area = 0;
        let mut fence_parts = vec![];
        while let Some(next) = flood_fill_queue.pop_front() {
            let already_seen = !seen.insert(next);
            if already_seen {
                continue;
            }
            area += 1;
            for d in RCDirection::four() {
                let neighbor = next + d;
                if grid.index_opt(neighbor) == Some(plant) {
                    flood_fill_queue.push_back(neighbor);
                } else {
                    fence_parts.push((next, d));
                }
            }
        }
        part1 += area * fence_parts.len();

        let mut unique_fences = fence_parts.len();
        for (candidate_fence_pos, direction) in &fence_parts {
            // a fence part is defined at a specific edge of a grid cell
            // in the planting area. a line of fence parts makes a fence.
            // in order to count fences, we try to count only the "first"
            // fence part of each fence.
            // that means if we see a fence part to the fence part's rhs,
            // we don't count this fence part
            let right = direction.clockwise();
            let possible_dupe_fence = *candidate_fence_pos + right;
            if fence_parts.contains(&(possible_dupe_fence, *direction)) {
                unique_fences -= 1;
            }
        }

        part2 += area * unique_fences;
    }

    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
AAAA
BBCD
BBCC
EEEC
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 140);
    assert_eq!(part2, 80);
    Ok(())
}
