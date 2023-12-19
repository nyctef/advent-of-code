use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use regex::Regex;
use std::str::FromStr;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 18)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let re = Regex::from_str(r"#(.{5})(.)")?;
    let lines = input
        .trim()
        .lines()
        .map(|l| re.captures(l).unwrap())
        .map(|c| {
            (
                c[2].chars().next().unwrap(),
                u64::from_str_radix(&c[1], 16).unwrap(),
            )
        })
        .collect_vec();

    dbg!(&lines);

    // let mut grid = XYGrid::new();
    let mut current = XYIndex::new(0, 0);
    // let mut updown = 'o';
    // grid.set(current, 'o');
    let mut updown_ranges = vec![];
    let mut max = XYIndex::new(0, 0);
    let mut min = XYIndex::new(0, 0);

    for (mut dir_char, count) in lines {
        let dir = match dir_char {
            '3' => XYDirection::up(),
            '2' => XYDirection::left(),
            '0' => XYDirection::right(),
            '1' => XYDirection::down(),
            _ => panic!("unrecognised direction {}", dir_char),
        };

        if dir_char == '1' {
            dir_char = 'D'
        }
        if dir_char == '3' {
            dir_char = 'U'
        }

        if dir_char == 'U' || dir_char == 'D' {
            let x = current.x;
            let ystart = current.y;
            let yend = current.y + (dir.ydiff * count as isize);
            let ymin = ystart.min(yend);
            let ymax = ystart.max(yend);
            // dbg!(&current, &dir, ystart, yend, ymin, ymax);
            // println!();
            updown_ranges.push((dir_char, x, ymin, ymax));
        }

        /*
        match dir_char {
            'U' => updown = 'U',
            'D' => updown = 'D',
            _ => {}
        };
        grid.set(current, updown);
        */
        current = current + (dir * count as isize);
        max = max.max(&current);
        min = min.min(&current);
    }

    // dbg!(&updown_ranges, max, min);

    let mut holes: u64 = 0;
    for y in min.y..=max.y {
        // println!();
        // println!("checking y={}", y);
        let mut matching_ranges = updown_ranges
            .iter()
            .filter(|r| y >= r.2 && y <= r.3)
            .collect_vec();
        matching_ranges.sort_by_key(|r| r.1);
        // println!("matching ranges: {:?}", matching_ranges);

        let mut parity = 0;
        let mut updown = ' ';
        let mut x_start = isize::min_value();
        let mut prev_was_start_end = ' ';
        let mut corner_count = 0;

        for (ud, x, ymin, ymax) in matching_ranges {
            // TODO: can't we pattern match these dereferences in the above line?
            let ud = *ud;
            let x = *x;
            let ymin = *ymin;
            let ymax = *ymax;
            let is_range_start = (ud == 'U' && y == ymin) || (ud == 'D' && y == ymax);
            let is_range_end = (ud == 'U' && y == ymax) || (ud == 'D' && y == ymin);
            let is_start_end = if is_range_start {
                'S'
            } else if is_range_end {
                'E'
            } else {
                'M'
            };
            assert!(!(is_range_start && is_range_end));
            // println!( "checking {} range at {} | se: {} | pse: {}", ud, x, is_start_end, prev_was_start_end);
            let is_corner = is_start_end == 'S' || is_start_end == 'E';
            if is_corner {
                corner_count += 1;
            }
            let is_ud_changing = ud != updown;
            // println!( "pwc: {} ic: {} cc: {} iudc: {}", prev_was_corner, is_corner, corner_count, is_ud_changing);
            // if we're outside the shape and we hit anything, then switch
            if (parity == 0)
                // so remaining checks assume we're inside the shape:
                //
                // if we hit a vertical wall then we're going outside
                || (!is_corner)
                // TODO: instead of these checks, try just checking for U vs S shape
                || (//prev_was_corner && is_corner
                    corner_count % 2 == 0
                    && !is_ud_changing
                    && (is_start_end == prev_was_start_end))
                || (
                    // prev_was_corner && is_corner
                    corner_count % 2 == 0
                    && is_ud_changing
                    && (is_start_end != prev_was_start_end))
            {
                // println!(" changing from {} to {}", updown, ud);
                // TODO: try changing `updown` here to `prev_updown` and always flipping it
                updown = ud;
                parity = 1 - parity;
                if parity == 1 {
                    // println!("  starting a line at {}", x);
                    // beginning of line to fill
                    x_start = x;
                } else {
                    let new_holes = (1 + x - x_start) as u64;
                    // println!("  ending a line at {} ({} new holes)", x, new_holes);
                    // end of line to fill
                    holes += new_holes;
                    x_start = x;
                }
            }

            /*
            else {
                println!(" no change");
                let new_holes = (x - x_start) as u64;
                println!("  continuing a line at {} ({} new holes)", x, new_holes);
                holes += new_holes;
                x_start = x;
            }
            */
            prev_was_start_end = is_start_end;
        }
    }

    /*
    let mut parity = 0;
    let mut updown = ' ';
    let mut prev_y = 99999;
    for (i, c) in grid.enumerate_chars_xy().collect_vec() {
        if i.y != prev_y {
            prev_y = i.y;
            parity = 0;
            updown = ' ';
        }

        if c == 'U' || c == 'D' {
            if c != updown {
                updown = c;
                parity = 1 - parity;
            }
        } else if c == ' ' {
            if parity == 1 {
                grid.set(i, '#');
            }
        }
    }
    */

    // println!("{}", grid);
    Ok(format!("holes: {holes}"))
}


#[test]
fn test_example1() -> Result<()> {
    let input = r###"
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"###;
    let result = solve_for(input)?;

    assert_eq!("holes: 952408144115", result);
    Ok(())
}

#[test]
fn test_example2() -> Result<()> {
    // made up example
    //    0123456
    //  0 ####### -> 7
    // -1 #     # -> 7
    // -2 # ### # -> 7
    // -3 # # # # -> 6
    // -4 ### ### -> 6
    let input = r###"
R 6 (#000060)
D 4 (#000041)
L 2 (#000022)
U 2 (#000023)
L 2 (#000022)
D 2 (#000021)
L 2 (#000022)
U 4 (#000043)
"###;
    let result = solve_for(input)?;

    assert_eq!("holes: 33", result);
    Ok(())
}

#[test]
fn test_example3() -> Result<()> {
    /*
      taken from a day10 example

       01234567890
    0  ########### -> 11
    -1 # ####### # -> 11
    -2 # #     # # -> 3 + 3 = 6
    -3 # #     # # -> 3 + 3 = 6
    -4 # ### ### # -> 5 + 5 = 10
    -5 #   # #   # -> 5 + 5 = 10
    -6 ##### ##### -> 5 + 5 = 10

    */
    let input = r###"
R 10 (#0000a0)
D 6 (#000061)
L 4 (#000042)
U 2 (#000023)
R 2 (#000020)
U 3 (#000033)
L 6 (#000062)
D 3 (#000031)
R 2 (#000020)
D 2 (#000021)
L 4 (#000042)
U 6 (#000063)
"###;
    let result = solve_for(input)?;

    assert_eq!("holes: 64", result);
    Ok(())
}

// TODO: stair-step test (plus shape?)
// TODO: inverse cityscape ( start with example2 but with two equal indents, then try differently
// sized indents if that passes)
