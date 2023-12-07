use crate::utils::*;
use color_eyre::eyre::Result;
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

    // we find the kind of a hand by sorting its individual characters, then
    // counting how long each run of a given character is.
    // eg KTKTK might sorted into TTKKK, which has duplicate counts of [2, 3]
    //    then [2, 3] gets sorted to [3, 2] which we can look up in the above list.
    let hand_kind = hand
        .chars()
        .sorted()
        .dedup_with_count()
        .map(|(c, _)| c)
        .sorted_by_key(|&c| Reverse(c))
        .collect_vec();
    let hand_kind_rank = hand_kinds
        .iter()
        .find(|(_, hk)| **hk == hand_kind)
        .unwrap_or_else(|| panic!("can't find hand kind for hand {hand} {hand_kind:?}"))
        .0;
    hand_kind_rank
}

fn solve_for(input: &str) -> Result<String> {
    let lines = input.trim().lines();

    let mut cards = [
        'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J',
    ];
    cards.reverse();
    let card_strength = cards.iter().enumerate().collect_vec();

    let mut players = lines
        .map(|l| {
            let (hand, bid) = l.split_once(' ').unwrap();
            let bid: u32 = bid.parse().unwrap();

            // to check the effect of the Joker we just try replacing it
            // with every other card to see what the best option would be
            let hand_kind_rank = cards
                .iter()
                .map(|c| hand_kind_rank(&hand.replace('J', &c.to_string())))
                .max()
                .unwrap();

            let card_ranks = hand
                .chars()
                .map(|c| card_strength.iter().find(|(_, cs)| **cs == c).unwrap().0)
                .collect_vec();

            // rust will conveniently sort this tuple of (hand_kind_rank, card_ranks)
            // in the way that we want - first sorting by the first element of the tuple,
            // then by the vec of card ranks: breaking ties by the first element of the vec,
            // then the second, and so on
            (bid, (hand_kind_rank, card_ranks))
        })
        .collect_vec();

    // this sort is ascending, so the weakest hands will be at the beginning of the resulting
    // vec (and so .enumerate() will give them a lower rank)
    players.sort_by_key(|i| i.1.clone());

    for i in &players {
        println!("{:?}", &i);
    }

    let sorted = players
        .iter()
        .enumerate()
        .map(|(r, i)| (r + 1) * i.0 as usize)
        .collect_vec();
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
