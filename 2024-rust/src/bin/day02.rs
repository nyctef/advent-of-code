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
    let mut safe_damped_report_count = 0;
    'report: for report in reports {
        if is_safe(&report) {
            safe_report_count += 1;
            continue;
        }

        // otherwise try removing one element and see if that makes it safe
        for removed_elem in 0..report.len() {
            let prefix = report.iter().copied().take(removed_elem);
            // take(999) will limit itself to however many elements are remaining rather than panic
            let suffix = report.iter().copied().skip(removed_elem + 1).take(999);
            let new_report = prefix.chain(suffix).collect_vec();

            if is_safe(&new_report) {
                safe_damped_report_count += 1;
                continue 'report;
            }
        }
    }

    let part1 = safe_report_count;
    let part2 = safe_report_count + safe_damped_report_count;
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn is_safe(report: &[i64]) -> bool {
    let diffs = report.windows(2).map(|w| w[1] - w[0]).collect_vec();
    let all_diffs_small = diffs.iter().all(|&d| d.abs() <= 3);
    let all_diffs_ascending = diffs.iter().all(|&d| d > 0);
    let all_diffs_descending = diffs.iter().all(|&d| d < 0);

    all_diffs_small && (all_diffs_ascending || all_diffs_descending)
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

    assert_eq!("Part 1: 2 | Part 2: 4", result);
    Ok(())
}
