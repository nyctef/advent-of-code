use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 9)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, u64)> {
    let map = all_digits(input.trim());
    // eprintln!("{:?}", &map);

    // scanning ptr reads from left to right over the files
    // moving ptr reads from right to left whenever the scanning ptr hits free space
    // both ptrs point at whole files inside the compacted map, so we need to also track the block
    // offsets within those ptrs
    //
    let mut scanning_ptr = 0;
    let mut scanning_ptr_offset = 0;
    let mut moving_ptr = map.len() - 1;
    let mut moving_ptr_offset = 0;

    let mut current_block = 0;

    let mut checksum = 0;
    loop {
        let is_scanning_free_space = scanning_ptr % 2 == 1;
        let file_id = if !is_scanning_free_space {
            scanning_ptr / 2
        } else {
            moving_ptr / 2
        };

        eprintln!(
            "scanning: {}/{} moving: {}/{} current block: {} file_id: {}",
            scanning_ptr,
            scanning_ptr_offset,
            moving_ptr,
            moving_ptr_offset,
            current_block,
            file_id
        );

        checksum += current_block * file_id;

        // we only move the file moving ptr when we're scanning free space to move it into
        if is_scanning_free_space {
            // if we're in free space, consume one file block off the end
            // the moving ptr should always be pointing at a file block
            assert!(moving_ptr % 2 == 0);
            loop {
                let &moving_file_size = &map[moving_ptr];
                moving_ptr_offset += 1;
                if moving_ptr_offset > moving_file_size {
                    moving_ptr_offset = 0;
                    moving_ptr -= 1;
                }

                let is_moving_file = moving_ptr % 2 == 0;
                if is_moving_file {
                    break;
                }
            }
        }

        // move scanning ptr forward one block
        let &scanning_file_size = &map[scanning_ptr];
        dbg!(&scanning_file_size);
        scanning_ptr_offset += 1;
        if scanning_ptr_offset > scanning_file_size {
            scanning_ptr_offset = 0;
            scanning_ptr += 1;
        }
        current_block += 1;

        // TODO: more exact edge cases
        eprintln!(
            "break? {}/{} vs {}/{}",
            scanning_ptr, scanning_ptr_offset, moving_ptr, moving_ptr_offset
        );
        if scanning_ptr >= moving_ptr
            && scanning_ptr_offset + moving_ptr_offset >= scanning_file_size
        {
            break;
        }
    }
    let part1 = checksum;
    let part2 = 0;
    Ok((part1, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
2333133121414131402
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 1928);
    assert_eq!(part2, 0);
    Ok(())
}

#[test]
fn test_2() -> Result<()> {
    let input = "101";
    let (part1, _) = solve_for(input)?;
    // file id 1 moves to position 1
    assert_eq!(part1, 1);
    Ok(())
}
#[test]
fn test_3() -> Result<()> {
    let input = "113";
    // 10222
    // 12220
    // =
    let (part1, _) = solve_for(input)?;

    assert_eq!(part1, 12);
    Ok(())
}
