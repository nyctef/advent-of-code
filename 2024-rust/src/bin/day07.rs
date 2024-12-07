use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::{repeat_n, Itertools};

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 7)?;

    let (part1, part2) = solve_for(&input)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Multiply,
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let calibrations = input
        .trim()
        .lines()
        .map(|l| l.split_once(':').unwrap())
        .map(|(target, values)| (target.parse::<u64>().unwrap(), all_numbers_u64(values)))
        .collect_vec();

    let mut calibration_result = 0;
    // dbg!(&calibrations);

    'next_calibration: for (target, values) in calibrations {
        eprintln!("{} {:?}", target, values);
        // choose a set of + and * operations to fit between the gaps of `values`
        let available_choices = vec![Operator::Add, Operator::Multiply];
        let choices = repeat_n(available_choices.iter(), values.len() - 1)
            .multi_cartesian_product()
            .collect_vec();
        // dbg!(&choices);

        for choice in choices.iter() {
            let mut total = values[0];
            let mut i = 1;
            for operator in choice {
                match operator {
                    Operator::Add => {
                        total += values[i];
                    }
                    Operator::Multiply => {
                        total *= values[i];
                    }
                };
                i += 1;
            }
            assert!(i == values.len());
            // eprintln!(
            //     "trying to match target {} with values {:?} choice {:?} : got {}",
            //     target, values, choice, total
            // );
            if total == target {
                // eprintln!(
                //     "found match for target {} : {:?} {:?}",
                //     target, values, choice
                // );
                calibration_result += total;
                continue 'next_calibration;
            }
        }
    }

    let part2 = 0;
    Ok((calibration_result, part2))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"###;
    let (part1, part2) = solve_for(input)?;

    assert_eq!(part1, 3749);
    assert_eq!(part2, 0);
    Ok(())
}
