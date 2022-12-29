use crate::aoc_util::*;
use crate::err_util::*;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 8)?;

    let layer_counts = input
        .trim()
        .chars()
        .map(|c| char::to_digit(c, 10).unwrap())
        .chunks(25 * 6)
        .into_iter()
        .map(|layer| layer.collect::<Vec<_>>())
        .map(|layer| layer.into_iter().counts())
        .min_by(|x, y| x.get(&0).cmp(&y.get(&0)))
        .unwrap();

    dbg!(layer_counts.get(&1).unwrap() * layer_counts.get(&2).unwrap());

    Ok(())
}

#[test]
fn test1() {
    // ...
}
