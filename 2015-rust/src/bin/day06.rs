use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    let input = get_input(2015, 6)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut grid = CharGrid::from_empty_char('.', 1000, 1000);

    for line in input.trim().lines() {
        let nums = all_numbers_usize(line);
        assert!(nums[0] <= nums[2] && nums[1] <= nums[3]);
        let (xmin, ymin, xmax, ymax) = nums.into_iter().collect_tuple().unwrap();
        if line.starts_with("turn off") {
            for x in xmin..=xmax {
                for y in ymin..=ymax {
                    grid.set_index_rc(CharGridIndexRC::new(x, y), '.');
                }
            }
        } else if line.starts_with("turn on") {
            for x in xmin..=xmax {
                for y in ymin..=ymax {
                    grid.set_index_rc(CharGridIndexRC::new(x, y), 'O');
                }
            }
        } else if line.starts_with("toggle") {
            for x in xmin..=xmax {
                for y in ymin..=ymax {
                    let pos = CharGridIndexRC::new(x, y);
                    let new_char = if grid[pos] == '.' { 'O' } else { '.' };
                    grid.set_index_rc(pos, new_char);
                }
            }
        } else {
            panic!("unrecognised instruction: {}", line);
        }
    }

    let part1 = grid.enumerate_chars_rc().filter(|(_, c)| c == &'O').count();
    let mut part2_grid = vec![[0_usize; 1000]; 1000];

    for line in input.trim().lines() {
        let nums = all_numbers_usize(line);
        assert!(nums[0] <= nums[2] && nums[1] <= nums[3]);
        let (xmin, ymin, xmax, ymax) = nums.into_iter().collect_tuple().unwrap();
        if line.starts_with("turn off") {
            for x in xmin..=xmax {
                for y in ymin..=ymax {
                    part2_grid[x][y] = part2_grid[x][y].saturating_sub(1);
                }
            }
        } else if line.starts_with("turn on") {
            for x in xmin..=xmax {
                for y in ymin..=ymax {
                    part2_grid[x][y] += 1;
                }
            }
        } else if line.starts_with("toggle") {
            for x in xmin..=xmax {
                for y in ymin..=ymax {
                    part2_grid[x][y] += 2;
                }
            }
        } else {
            panic!("unrecognised instruction: {}", line);
        }
    }
    let part2 = part2_grid.iter().flat_map(|g| g.iter()).sum::<usize>();
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}
