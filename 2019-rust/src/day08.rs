use crate::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 8)?;

    let layers = input
        .trim()
        .chars()
        .map(|c| char::to_digit(c, 10).unwrap())
        .chunks(25 * 6)
        .into_iter()
        .map(|layer| layer.collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut result = vec![0; 25 * 6];

    for i in 0..(25 * 6) {
        for layer in layers.iter() {
            if layer[i] == 2 {
                continue;
            }
            result[i] = layer[i];
            break;
        }
    }

    for line in result.chunks(25) {
        for pixel in line {
            print!("{}", if *pixel == 0 { '█' } else { ' ' })
        }
        println!()
    }

    Ok(())
}

#[test]
fn test1() {
    // ...
}
