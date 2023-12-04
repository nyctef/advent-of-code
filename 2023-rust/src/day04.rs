use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::collections::HashSet;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 4)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn all_numbers(input: &str) -> Vec<u32> {
    regex::Regex::new(r"\d+")
        .unwrap()
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}

fn solve_for(input: &str) -> Result<String> {
    let mut point_total = 0;

    let mut card_list: Vec<u32> = Vec::new();

    let lines = input.trim().lines().collect_vec();
    for line in &lines {
        let (num, line) = line.split_once(':').unwrap();
        // let num = all_numbers(num).iter().exactly_one();

        let (winners, have) = line.split_once('|').unwrap();
        let (winners, have) = (all_numbers(winners), all_numbers(have));
        let winners: HashSet<_> = HashSet::from_iter(winners);
        let have: HashSet<_> = HashSet::from_iter(have);
        let in_common: HashSet<_> = winners.intersection(&have).collect();
        let num_in_common: usize = in_common.len();

        if num_in_common > 0 {
            // dbg!(in_common, num_in_common, 2u32.pow(num_in_common as u32 - 1));
            point_total += 2u32.pow(num_in_common as u32 - 1);
        }

        card_list.push(num_in_common as u32);
    }

    let mut card_counts: Vec<u32> = Vec::from_iter(std::iter::repeat(1).take(lines.len()));

    for (card_index, &num_in_common_for_card) in card_list.iter().enumerate() {
        println!("considering {card_index} with {num_in_common_for_card} matches");
        if num_in_common_for_card > 0 {
            // println!(
            //     "updating card counts for {} to {} with {}",
            //     (card_index + 1),
            //     (card_index + (num_in_common_for_card as usize)),
            //     card_counts[card_index]
            // );
            for future_card in
                (card_index + 1)..(card_index + (num_in_common_for_card as usize) + 1)
            {
                card_counts[future_card] += card_counts[card_index];
            }
            // dbg!(&card_counts);
        }
    }
    // dbg!(&card_list, &card_counts);

    let card_total: u32 = card_counts.iter().sum();

    Ok(format!("Part 1: {point_total} | Part 2: {card_total}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 13 | Part 2: 30", result);
    Ok(())
}
