use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 9)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let input = input.trim().lines();
    let sequences = input.map(|l| all_numbers_i64(l)).collect_vec();
    let part1: i64 = 0;

    for seq in &sequences {
        let layers = extract_layers(seq);

        dbg!(&layers);
        for i in 0..7 {
            println!("{}: {}", i, calc_taylor_series_at_1_for_x(&layers, i))
        }
    }

    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn extract_layers(seq: &Vec<i64>) -> Vec<Vec<i64>> {
    let mut seq = seq.clone();
    let mut layers = vec![seq.clone()];
    loop {
        let mut next_layer = vec![];
        for i in 1..seq.len() {
            next_layer.push(seq[i] - seq[i - 1]);
        }
        if next_layer.iter().all(|x| x == &0) {
            break;
        }
        layers.push(next_layer);
        seq = layers.iter().last().unwrap().clone();
    }
    assert!(layers.iter().last().iter().dedup().count() == 1);
    layers
}

fn calc_taylor_series_at_1_for_x(layers: &[Vec<i64>], x: i64) -> f64 {
    let mut total: f64 = layers[0][1] as f64;
    // dbg!(total);
    // for n layers, we expect an order n-1 polynomial
    // we set a=1 for https://en.wikipedia.org/wiki/Taylor_series#Definition
    for l in 1..layers.len() {
        let derivative = calc_taylor_series_derivative_at_1(&layers[l..]);
        let divider = factorial(l as i64) as f64;
        let x_pow = (x - 1).pow(l as u32);
        total += x_pow as f64 * (derivative / divider);
        println!(
            "got total {} after computing pow {} (x-1)^pow={} derivative={} divider = {}",
            &total, l, x_pow, derivative, divider
        );
    }
    total
}

fn calc_taylor_series_derivative_at_1(layers: &[Vec<i64>]) -> f64 {
    // we assume we've skipped the first layer of the original sequence.

    if layers.len() == 0 {
        // just a constant sequence
        return 0 as f64;
    }
    if layers.len() == 1 {
        // for linear sequences, the derivative of the whole thing is just the bottom layer
        return layers[0][0] as f64;
    }
    if layers.len() == 2 {
        // just an approximation, but pretty sure it's correct for quadratics
        return (layers[0][1] + layers[0][0]) as f64 / 2_f64;
    }

    // let mut total =

    0_f64
}

fn factorial(num: i64) -> i64 {
    (1..=num).product()
}

#[test]
fn extract_layers_for_linear_sequence() {
    let input = vec![0, 3, 6, 9, 12];
    assert_eq!(
        extract_layers(&input),
        vec![vec![0, 3, 6, 9, 12], vec![3, 3, 3, 3]]
    );
}
#[test]
fn extract_layers_for_quadratic_sequence() {
    let input = vec![1, 3, 6, 10, 15];
    assert_eq!(
        extract_layers(&input),
        vec![vec![1, 3, 6, 10, 15], vec![2, 3, 4, 5], vec![1, 1, 1,]]
    );
}
#[test]
fn extract_layers_for_cubic_sequence() {
    let input = vec![10, 13, 16, 21, 30];
    assert_eq!(
        extract_layers(&input),
        vec![
            vec![10, 13, 16, 21, 30],
            vec![3, 3, 5, 9,],
            vec![0, 2, 4],
            vec![2, 2]
        ]
    )
}

#[test]
fn calc_taylor_series_for_linear_sequence() {
    let input = vec![0, 3, 6, 9, 12];
    assert_eq!(
        calc_taylor_series_at_1_for_x(&extract_layers(&input), 5),
        15_f64
    );
}
#[test]
fn calc_taylor_series_for_quadratic_sequence() {
    let input = vec![1, 3, 6, 10, 15];
    assert_eq!(
        calc_taylor_series_at_1_for_x(&extract_layers(&input), 5),
        21_f64
    );
}
#[test]
fn calc_taylor_series_for_quadratic_sequence_2() {
    let input = vec![3, 3, 5, 9];
    let series = (0..6)
        .map(|x| calc_taylor_series_at_1_for_x(&extract_layers(&input), x) as i64)
        .collect_vec();
    assert_eq!(series, vec![3, 3, 5, 9, 15, 23]);
}

#[test]
fn calc_taylor_series_for_cubic_sequence() {
    let input = vec![10, 13, 16, 21, 30];
    assert_eq!(
        calc_taylor_series_at_1_for_x(&extract_layers(&input), 5),
        45_f64
    );
}

#[test]
fn calc_taylor_series_derivative_for_linear_sequence() {
    let input = vec![0, 3, 6, 9, 12];
    assert_eq!(
        calc_taylor_series_derivative_at_1(&extract_layers(&input)[1..]),
        3_f64
    );
}
#[test]
fn calc_taylor_series_derivative_for_quadratic_sequence() {
    let input = vec![1, 3, 6, 10, 15];
    assert_eq!(
        calc_taylor_series_derivative_at_1(&extract_layers(&input)[1..]),
        2.5_f64
    );
}
#[test]
fn calc_taylor_series_derivative_for_quadratic_sequence_2() {
    let input = vec![3, 3, 5, 9];
    assert_eq!(
        calc_taylor_series_derivative_at_1(&extract_layers(&input)[1..]),
        1_f64
    );
}
#[test]
#[allow(unreachable_code)]
fn calc_taylor_series_derivative_for_cubic_sequence() {
    let input = vec![10, 13, 16, 21, 30];

    // this sequence splits into the following layers:
    // [10, 13, 16, 21, 30],
    //   [3, 3, 5, 9,],
    //     [0, 2, 4],
    //      [2, 2]
    // the value of the top sequence at index 1 depends on (its own?) derivative
    // let's start over
    //
    // the taylor series for the bottom layer [2,2] is f(x) = f(a=1) = 2
    // the taylor series for the third layer [0, 2, 4] with a=1 is
    //   f(x) = f(a) + f'(a)/1(x-a)
    //        = 2 + 2/1(x-1)
    //        = 2x
    // the taylor series for the second layer [3, 3, ...] with a=1 is
    //   f(x) = f(a) + f'(a)/1(x-a) + f''(a)/2!(x-a)^2
    //        = 3 + 1?(x-1) + 2/2(x-1)^2
    //        = 3 + x - 1 + (x^2 - 2x + 1)
    //        = 2 + x + x^2 - 2x + 1
    //        = 3 -x + x^2
    //        => [3, 3, 5, 9 ...]
    // the derivative of the above series for the second layer with a=1 is
    //  f'(x) = f'(a) + f''(a)/1!(x-a)
    //        = 1 + 2(x-1) = 2x - 1
    //        => for the next part we only need the derivative at x=a=1 though so a lot of terms cancel out
    // the derivative of the taylor series for the first layer with a=1 is:
    //  f'(x) = ...
    // the taylor series for the first layer [10, 13, 16...] with a=1 is
    //   f(x) = f(a) + f'(a)/1(x-a) + f''(a)/2!(x-a)^2 + f'''(a)/3!(x-a)^3
    //        = 13 + ...

    assert_eq!(
        calc_taylor_series_derivative_at_1(&extract_layers(&input)[1..]),
        todo!() as f64
    );
}

#[test]
#[ignore]
fn test_example1() -> Result<()> {
    let input = r###"
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 114 | Part 2: ", result);
    Ok(())
}
