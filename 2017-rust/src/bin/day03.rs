use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 3)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (i64, u64) {
    let target: u64 = input.trim().parse().unwrap();
    let (x, y) = pos(target);
    let mut part1 = x.abs() + y.abs();
    let mut part2 = 0;

    (part1, part2)
}

fn pos(n: u64) -> (i64, i64) {
    // modeling a spiral like:
    //
    // 17  16  15  14  13
    // 18   5   4   3  12
    // 19   6   1   2  11
    // 20   7   8   9  10
    // 21  22  23---> ...
    //
    // we move right one, then up one,
    // then left two, then down two,
    // then right three, then up three,
    // and so on
    //
    let mut count = 1;
    let mut pos_x = 0;
    let mut pos_y = 0;
    let mut dir_x = 1;
    let mut dir_y = 0;
    let mut dist = 1;
    let mut steps = dist;
    let mut dir_change_count = 2;
    // eprintln!("===");
    while count != n {
        if steps == 0 {
            steps = dist;
            (dir_x, dir_y) = (dir_y, -dir_x);
            dir_change_count -= 1;
        }
        if dir_change_count == 0 {
            dist += 1;
            steps = dist;
            dir_change_count = 2;
        }

        pos_x += dir_x;
        pos_y += dir_y;
        count += 1;
        steps -= 1;
        // eprintln!("count={count} pos_x={pos_x} pos_y={pos_y} dir_x={dir_x} dir_y={dir_y} dist={dist} steps={steps} dir_change_count={dir_change_count}");
    }

    (pos_x, pos_y)
}

#[test]
fn test_pos() {
    assert_eq!(pos(1), (0, 0));
    assert_eq!(pos(2), (1, 0));
    assert_eq!(pos(6), (-1, 0));
    assert_eq!(pos(23), (0, 2));
}

#[test]
fn test_example1() {
    let input = r###"
    
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 0);
    assert_eq!(part2, 0);
}
