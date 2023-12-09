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

        dbg!(&layers);
        for i in 0..7 {
            println!("{}: {}", i, calc_taylor_series_at_1_for_x(&layers, i))
        }
    }

    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn calc_taylor_series_at_1_for_x(layers: &Vec<Vec<i64>>, x: i64) -> i64 {
    let mut total: f64 = layers[0][1] as f64;
    // dbg!(total);
    // for n layers, we expect an order n-1 polynomial
    // we set a=1 for https://en.wikipedia.org/wiki/Taylor_series#Definition
    for l in 1..layers.len() {
        let derivative = (layers[l][1] + layers[l][0]) as f64 / 2_f64;
        let divider = factorial(l as i64) as f64;
        let x_pow = (x - 1).pow(l as u32);
        total += x_pow as f64 * (derivative / divider);
        println!(
            "got total {} after computing pow {} (x-1)^pow={} derivative={} divider = {}",
            &total, l, x_pow, derivative, divider
        );
    }
    total as i64
}

fn factorial(num: i64) -> i64 {
    (1..=num).product()
}

#[test]
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
