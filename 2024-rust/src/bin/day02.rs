use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 2)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let reports = input.trim().lines().map(all_numbers_i64).collect_vec();
    let mut safe_report_count = 0;
    for report in reports {
        let all_diffs_small = report
            .windows(2)
            .map(|w| (w[1] - w[0]).abs() <= 3)
            .all(|b| b);
        let all_diffs_ascending = report.windows(2).map(|w| w[0] < w[1]).all(|b| b);
        let all_diffs_descending = report.windows(2).map(|w| w[0] > w[1]).all(|b| b);

        dbg!(
            &report,
            &all_diffs_small,
            &all_diffs_ascending,
            &all_diffs_descending
        );

        if all_diffs_small && (all_diffs_ascending || all_diffs_descending) {
            safe_report_count += 1;
        }
    }

    let part1 = safe_report_count;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 2 | Part 2: ", result);
    Ok(())
}
