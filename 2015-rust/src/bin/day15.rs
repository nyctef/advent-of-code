use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 15)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let ingredients = input
        .trim()
        .lines()
        .map(|l| {
            all_numbers_isize(l)
                .into_iter()
                .collect_tuple::<(_, _, _, _, _)>()
                .unwrap()
        })
        .collect_vec();
    // dbg!(&ingredients);

    let mut part1 = 0;
    let choices = (0..=100 as usize).permutations(ingredients.len());

    for choice in choices {
        if choice.iter().sum::<usize>() != 100 {
            continue;
        }
        let res = choice.iter().enumerate().map(|(i, c)| {
            let ingredient = ingredients[i];
            mul_ingredient(ingredient, *c as isize)
        }).fold((0,0,0,0,0), |a, n| add_ingredient(a, n));

        let total = res.0.max(0) * res.1.max(0) * res.2.max(0) * res.3.max(0);
        if total > part1 {
            println!("choice {:?} produced score {} from parts {:?}", &choice, total, res);
            part1 = total;
        }
    }

    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}
fn add_ingredient(
    ingr1: (isize, isize, isize, isize, isize),
    ingr2: (isize, isize, isize, isize, isize)
) -> (isize, isize, isize, isize, isize) {
    (
        ingr1.0 + ingr2.0,
        ingr1.1 + ingr2.1,
        ingr1.2 + ingr2.2,
        ingr1.3 + ingr2.3,
        ingr1.4 + ingr2.4,
    )
}

fn mul_ingredient(
    ingr: (isize, isize, isize, isize, isize),
    val: isize,
) -> (isize, isize, isize, isize, isize) {
    (
        ingr.0 * val,
        ingr.1 * val,
        ingr.2 * val,
        ingr.3 * val,
        ingr.4 * val,
    )
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 62842880 | Part 2: ", result);
    Ok(())
}
