use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 3)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let target: u64 = input.trim().parse().unwrap();
    let pos = pos(target);
    let part1 = PointXY::zero().to(pos).manhattan();
    let part2 = part2(target);

    (part1, part2)
}

fn pos(n: u64) -> PointXY {
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
    let mut pos = PointXY::zero();
    let mut dir = YDown::right();
    let mut dist = 1;
    let mut steps = dist;
    let mut dir_change_count = 2;
    // eprintln!("===");
    while count != n {
        if steps == 0 {
            steps = dist;
            dir = YDown::counterclockwise(dir);
            dir_change_count -= 1;
        }
        if dir_change_count == 0 {
            dist += 1;
            steps = dist;
            dir_change_count = 2;
        }

        pos += dir;
        count += 1;
        steps -= 1;
        // eprintln!("count={count} pos_x={pos_x} pos_y={pos_y} dir_x={dir_x} dir_y={dir_y} dist={dist} steps={steps} dir_change_count={dir_change_count}");
    }

    pos
}

fn part2(n: u64) -> u64 {
    let mut filled_values = FxHashMap::default();
    filled_values.entry(PointXY::zero()).insert_entry(1);

    let mut pos = PointXY::zero();
    let mut dir = YDown::right();
    let mut dist = 1;
    let mut steps = dist;
    let mut dir_change_count = 2;
    loop {
        if steps == 0 {
            steps = dist;
            dir = YDown::counterclockwise(dir);
            dir_change_count -= 1;
        }
        if dir_change_count == 0 {
            dist += 1;
            steps = dist;
            dir_change_count = 2;
        }

        pos += dir;
        steps -= 1;

        let mut square_value = 0;
        for &d in DirXY::eight() {
            if let Some(neighbor) = filled_values.get(&(pos + d)) {
                square_value += neighbor;
            }
        }
        if square_value > n {
            return square_value;
        }
        filled_values.insert(pos, square_value);
    }
}

#[test]
fn test_pos() {
    assert_eq!(pos(1), PointXY::new(0, 0));
    assert_eq!(pos(2), PointXY::new(1, 0));
    assert_eq!(pos(6), PointXY::new(-1, 0));
    assert_eq!(pos(23), PointXY::new(0, 2));
}
