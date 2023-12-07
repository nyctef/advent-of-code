use crate::utils::*;
use color_eyre::eyre::{eyre, Result};
use itertools::Itertools;
use std::cmp::Reverse;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 7)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn hand_kind_rank(hand: &str) -> usize {
    let mut hand_kinds: [Vec<usize>; 7] = [
        vec![5],
        vec![4, 1],
        vec![3, 2],
        vec![3, 1, 1],
        vec![2, 2, 1],
        vec![2, 1, 1, 1],
        vec![1, 1, 1, 1, 1],
    ];
    hand_kinds.reverse();
    let hand_kinds = hand_kinds.iter().enumerate().collect_vec();

    let hand_kind = hand
        .chars()
        .sorted()
        .dedup_with_count()
        .map(|(c, _)| c)
        .sorted_by_key(|&c| Reverse(c))
        .collect_vec();
    let hand_kind_rank = hand_kinds
        .iter()
        .find(|(r, hk)| **hk == hand_kind)
        .unwrap_or_else(|| panic!("can't find hand kind for hand {hand} {hand_kind:?}"))
        .0;
    hand_kind_rank
}

fn solve_for(input: &str) -> Result<String> {
    let lines = input.trim().lines();

    let mut cards = [
        'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2',
    ];
    cards.reverse();
    let card_strength = cards.iter().enumerate().collect_vec();

    let mut inputs = lines
        .map(|l| {
            let (hand, bid) = l.split_once(' ').unwrap();
            let bid: u32 = bid.parse().unwrap();
            // println!("hand {hand} with bid {bid}");

            let hand_kind_rank = cards
                .iter()
                .map(|c| hand_kind_rank(&hand.replace('J', &c.to_string())))
                .max();

            let card_ranks = hand
                .chars()
                .map(|c| card_strength.iter().find(|(r, cs)| **cs == c).unwrap().0)
                .collect_vec();
            // println!("{hand} {bid} {hand_kind:?}, {hand_kind_rank}, {card_ranks:?}");
            (bid, (hand_kind_rank, card_ranks))
        })
        .collect_vec();

    inputs.sort_by_key(|i| i.1.clone());
    // inputs.reverse();

    let sorted = inputs
        .iter()
        .enumerate()
        .map(|(r, i)| (r + 1) * i.0 as usize)
        .collect_vec();
    // println!("{:?}", &sorted);
    let winnings: usize = sorted.iter().sum();

    Ok(format!("{winnings}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"###;
    let result = solve_for(input)?;

    assert_eq!("5905", result);
    Ok(())
}
