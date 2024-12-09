use std::iter;

use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 9)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> Result<(usize, usize)> {
    let map = all_digits(input.trim());
    // eprintln!("{:?}", &map);
    //
    /*

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

        eprintln!();
        eprintln!(
            "scanning: {}/{} moving: {}/{} current block: {} file_id: {} sfs: {}",
            scanning_ptr,
            scanning_ptr_offset,
            moving_ptr,
            moving_ptr_offset,
            current_block,
            file_id,
            is_scanning_free_space
        );


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

        checksum += current_block * file_id;

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
            "break? s{}/{} vs m{}/{} (file_id: {file_id} checksum: {checksum})",
            scanning_ptr, scanning_ptr_offset, moving_ptr, moving_ptr_offset
        );
        // if scanning_ptr is looking at a file past moving_ptr: break
        // if scanning_ptr and moving_ptr are looking at the same file, and we've
        // consumed all the blocks in that file: break
        if scanning_ptr >= moving_ptr
            && scanning_ptr_offset + moving_ptr_offset + 2 >= scanning_file_size
        {
            break;
        }
    }
    let part1 = checksum;

    */

    assert!(
        map.len() % 2 == 1,
        "the map len should be odd, since we assume the last entry in the map is a file"
    );

    let mut drive = make_drive(&map);

    for i in 0..drive.len() {
        if i >= drive.len() {
            break;
        }

        while drive[i] == u32::MAX {
            drive[i] = drive.pop().unwrap();
        }
    }
    assert!(
        drive.iter().all(|&x| x != u32::MAX),
        "we should have filled all the space"
    );
    let part1 = checksum(drive);

    let mut drive2 = make_drive(&map);
    // we try to move the last file first
    let mut file_id_to_move = drive2[drive2.len() - 1];
    while file_id_to_move > 0 {
        let (file, file_length) = find_file_by_id(&drive2, file_id_to_move);
        let maybe_space = find_space_with_size(&drive2, file_length);

        // have we found space to the left?
        if let Some(space) = maybe_space {
            if space < file {
                move_blocks(&mut drive2, file, file_length, space);
            }
        }

        // look for the next file
        file_id_to_move -= 1;
    }

    let part2 = checksum(drive2);
    Ok((part1, part2))
}

fn move_blocks(
    drive2: &mut [u32],
    beginning_of_file: usize,
    file_length: usize,
    beginning_of_space: usize,
) {
    for i in 0..file_length {
        drive2[beginning_of_space + i] = drive2[beginning_of_file + i];
        drive2[beginning_of_file + i] = u32::MAX;
    }
}

fn find_space_with_size(drive2: &[u32], file_length: usize) -> Option<usize> {
    let mut beginning_of_space_ptr = 0;
    loop {
        while beginning_of_space_ptr < drive2.len() && drive2[beginning_of_space_ptr] != u32::MAX {
            beginning_of_space_ptr += 1;
        }
        if beginning_of_space_ptr >= drive2.len() {
            break;
        }
        let mut end_of_space_ptr = beginning_of_space_ptr;
        while end_of_space_ptr + 1 < drive2.len() && drive2[end_of_space_ptr + 1] == u32::MAX {
            end_of_space_ptr += 1;
        }
        let space_length = end_of_space_ptr + 1 - beginning_of_space_ptr;

        if space_length >= file_length {
            break;
        }

        beginning_of_space_ptr = end_of_space_ptr + 1
    }

    // have we gone off the end of the drive?
    if beginning_of_space_ptr < drive2.len() {
        Some(beginning_of_space_ptr)
    } else {
        None
    }
}

fn find_file_by_id(drive2: &[u32], file_id_to_move: u32) -> (usize, usize) {
    // TODO: search from the previous file location rather than the end of the disk every time
    let mut file_to_move_ptr = drive2.len() - 1;
    while file_to_move_ptr > 0 && drive2[file_to_move_ptr] != file_id_to_move {
        file_to_move_ptr -= 1;
    }
    // find the beginning of this file
    let mut beginning_of_file = file_to_move_ptr;
    loop {
        if beginning_of_file == 0 {
            break;
        }
        if let Some(&p) = drive2.get(beginning_of_file - 1) {
            if p == file_id_to_move {
                beginning_of_file -= 1;
                continue;
            }
        }
        break;
    }
    let file_length = file_to_move_ptr + 1 - beginning_of_file;
    (beginning_of_file, file_length)
}

fn checksum(drive: Vec<u32>) -> usize {
    let mut checksum = 0;
    for (i, &file_id) in drive.iter().enumerate() {
        if file_id == u32::MAX {
            continue;
        }
        checksum += i * (file_id as usize);
    }
    checksum
}

fn make_drive(map: &[u32]) -> Vec<u32> {
    let mut drive = vec![];
    for (i, &segment) in map.iter().enumerate() {
        let is_file = i % 2 == 0;
        let file_id = i as u32 / 2;

        if is_file {
            drive.extend(iter::repeat(file_id).take(segment as usize));
        } else {
            drive.extend(iter::repeat(u32::MAX).take(segment as usize));
        }
    }
    drive
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
2333133121414131402
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 1928);
    assert_eq!(part2, 2858);
    Ok(())
}

#[test]
fn test_one_file_shift() -> Result<()> {
    let input = "111";
    let (part1, _) = solve_for(input)?;
    // 0.1
    // 01.
    assert_eq!(part1, 1);
    Ok(())
}

#[test]
fn test_larger_file_shift() -> Result<()> {
    let input = "113";
    // 0.111
    // 0111.
    // = 1*0 + 1*1 + 1*2 + 1*3
    let (part1, _) = solve_for(input)?;

    assert_eq!(part1, 6);
    Ok(())
}

#[test]
fn test_no_gaps() -> Result<()> {
    let input = "10101";
    // 012
    // 012
    // = 0*0 + 1*1 + 2*2
    let (part1, _) = solve_for(input)?;

    assert_eq!(part1, 5);
    Ok(())
}
