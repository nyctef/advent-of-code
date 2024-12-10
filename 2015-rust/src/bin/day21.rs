use std::ops::Add;

use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 21)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Constructor)]
struct Item {
    cost: u16,
    damage: u16,
    armor: u16,
}

fn weapons() -> Vec<Item> {
    vec![
        Item::new(8, 4, 0),
        Item::new(10, 5, 0),
        Item::new(25, 6, 0),
        Item::new(40, 7, 0),
        Item::new(74, 8, 0),
    ]
}

fn armor() -> Vec<Item> {
    vec![
        Item::new(0, 0, 0),
        Item::new(13, 0, 1),
        Item::new(31, 0, 2),
        Item::new(53, 0, 3),
        Item::new(75, 0, 4),
        Item::new(102, 0, 5),
    ]
}
fn rings() -> Vec<Item> {
    vec![
        Item::new(0, 0, 0),
        Item::new(25, 1, 0),
        Item::new(50, 2, 0),
        Item::new(100, 3, 0),
        Item::new(20, 0, 1),
        Item::new(40, 0, 2),
        Item::new(80, 0, 3),
    ]
}

// TODO: do we need both these impls?
impl Add<&Item> for &Item {
    type Output = Item;

    fn add(self, rhs: &Item) -> Self::Output {
        Item {
            cost: self.cost + rhs.cost,
            damage: self.damage + rhs.damage,
            armor: self.armor + rhs.armor,
        }
    }
}
impl Add<&Item> for Item {
    type Output = Item;

    fn add(self, rhs: &Item) -> Self::Output {
        Item {
            cost: self.cost + rhs.cost,
            damage: self.damage + rhs.damage,
            armor: self.armor + rhs.armor,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Constructor, Clone)]
struct Stats {
    hitpoints: u16,
    damage: u16,
    armor: u16,
}

impl Stats {
    fn attack(&self, other: &mut Stats) {
        let damage = (self.damage.saturating_sub(other.armor)).max(1);
        other.hitpoints = other.hitpoints.saturating_sub(damage);
    }
}

fn simulate(mut player: Stats, mut boss: Stats) -> bool {
    loop {
        player.attack(&mut boss);
        if boss.hitpoints == 0 {
            return true;
        }

        // eprintln!("boss: {:?}", boss);

        boss.attack(&mut player);
        if player.hitpoints == 0 {
            return false;
        }

        // eprintln!("player: {:?}", player);
    }
}

fn solve_for(input: &str) -> Result<String> {
    let boss_numbers = input
        .trim()
        .lines()
        .map(|l| all_numbers_u16(l).into_iter().exactly_one().unwrap())
        .collect_vec();
    let boss_stats = Stats::new(boss_numbers[0], boss_numbers[1], boss_numbers[2]);

    let mut lowest_cost = u16::MAX;
    for weapon in &weapons() {
        for armor in &armor() {
            for ring1 in &rings() {
                for ring2 in &rings() {
                    if ring1 == ring2 {
                        continue;
                    }

                    let player_stats = weapon + armor + ring1 + ring2;

                    if player_stats.cost >= lowest_cost {
                        continue;
                    }

                    let player = Stats::new(100, player_stats.armor, player_stats.damage);
                    let boss = boss_stats.clone();
                    if simulate(player, boss) {
                        lowest_cost = player_stats.cost;
                    }
                }
            }
        }
    }

    // let mut example_player = Stats::new(8, 5, 5);
    // let mut example_boss = Stats::new(12, 7, 2);
    // dbg!(simulate(example_player, example_boss));
    let part1 = lowest_cost;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
    
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: | Part 2: ", result);
    Ok(())
}
