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
    Concatenate,
}

fn solve_for(input: &str) -> Result<(u64, u64)> {
    let calibrations = input
        .trim()
        .lines()
        .map(|l| l.split_once(':').unwrap())
        .map(|(target, values)| (target.parse::<u64>().unwrap(), all_numbers_u64(values)))
        .collect_vec();

    let part_1_choices = vec![Operator::Add, Operator::Multiply];
    let calibration_result = calibrate(&calibrations, part_1_choices);

    let part_2_choices = vec![Operator::Add, Operator::Multiply, Operator::Concatenate];
    let calibration_result_2 = calibrate(&calibrations, part_2_choices);

    Ok((calibration_result, calibration_result_2))
}

fn calibrate(calibrations: &Vec<(u64, Vec<u64>)>, available_choices: Vec<Operator>) -> u64 {
    let mut calibration_result = 0;
    'next_calibration: for (target, values) in calibrations {
        // choose a set of operations to fit between the gaps of `values`
        //
        // from docs in itertools::permutations: apparently this is how you do
        // permutations with replacement
        let choices = repeat_n(available_choices.iter(), values.len() - 1)
            .multi_cartesian_product()
            .collect_vec();

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
                    Operator::Concatenate => {
                        // we need to give the left number (the current total)
                        // enough trailing zeros to fit the right number, which
                        // can then be added to it
                        //
                        let next = values[i];
                        let zeros = next.ilog10() + 1;
                        total *= 10_u64.pow(zeros);
                        total += next;
                    }
                };
                i += 1;
            }
            assert!(i == values.len());

            if total == *target {
                calibration_result += total;
                continue 'next_calibration;
            }
        }
    }
    calibration_result
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
    assert_eq!(part2, 11387);
    Ok(())
}
