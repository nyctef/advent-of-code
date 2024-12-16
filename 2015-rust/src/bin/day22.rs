use std::num::Saturating;

use aoc_2015_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2015, 22)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

#[derive(Constructor, Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Boss {
    hp: Saturating<u16>,
    dmg: u16,
}

#[derive(Constructor, Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Player {
    hp: Saturating<u16>,
    mana: Saturating<u16>,
    armor: u8,
}

// #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
// enum Ability {

// }
//

#[derive(Default, Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Effects {
    shield: Saturating<u8>,
    poison: Saturating<u8>,
    recharge: Saturating<u8>,
}

#[derive(Constructor, Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct State {
    player: Player,
    boss: Boss,
    effects: Effects,
    mana_spent: u16,
}

impl State {
    fn try_spend_mana(&self, cost: u16) -> Option<State> {
        if self.player.mana.0 >= cost {
            let mut player = self.player.clone();
            player.mana -= cost;
            Some(State::new(
                player,
                self.boss,
                self.effects,
                self.mana_spent + cost,
            ))
        } else {
            None
        }
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // the priority is minimizing mana spent
        self.mana_spent.cmp(&other.mana_spent)

        // is the rest of this useful, or is it just an a* heuristic?
        // .then(
        // self.player
        //     .mana
        //     .cmp(&other.player.mana)
        //     .then(self.player.hp.cmp(&other.player.hp))
        //     .then(self.effects.shield.cmp(&other.effects.shield))
        //     .then(self.effects.poison.cmp(&other.effects.poison))
        //     .then(self.effects.recharge.cmp(&other.effects.recharge))
        //     // higher stats are better, so they should count as lower distance for the purposes of
        //     // dijkstra
        //     .reverse(),
        // )
    }
}

fn solve_for(input: &str) -> Result<String> {
    let boss_stats = input
        .trim()
        .lines()
        .map(all_numbers)
        .map(|v| v[0])
        .collect_vec();
    let boss = Boss::new(
        Saturating(boss_stats[0].try_into().unwrap()),
        boss_stats[1].try_into().unwrap(),
    );
    // let boss = Boss::new(Saturating(13), 8);

    // let player = Player::new(Saturating(10), Saturating(250), 0);
    let player = Player::new(Saturating(50), Saturating(500), 0);

    let state = State::new(player, boss, Effects::default(), 0);

    let mut search = Dijkstra::new(|s: &State| (s.boss.hp, s.player, s.effects));
    search.push(state);

    let is_target_state = |s: &State| s.boss.hp.0 == 0 && s.player.hp.0 != 0;

    let apply_turn_start_effects = |s: &mut State| {
        s.player.armor = if s.effects.shield.0 > 0 { 7 } else { 0 };
        if s.effects.poison.0 > 0 {
            s.boss.hp -= 3;
        }
        if s.effects.recharge.0 > 0 {
            s.player.mana += 101;
        }

        s.effects.shield -= 1;
        s.effects.poison -= 1;
        s.effects.recharge -= 1;
    };

    let get_next_candidates = |mut s: State| {
        let mut nexts = vec![];
        // eprintln!("s: {:?}", s);

        if s.player.hp.0 == 0 {
            return nexts;
        }

        // player turn
        apply_turn_start_effects(&mut s);

        // eprintln!("after ts: {:?}", s);

        // magic missle
        if let Some(mut n) = s.try_spend_mana(53) {
            n.boss.hp -= 4;
            nexts.push(n);
        }

        // drain
        if let Some(mut n) = s.try_spend_mana(73) {
            n.boss.hp -= 2;
            n.player.hp += 2;
            nexts.push(n);
        }

        // shield
        if let Some(mut n) = s.try_spend_mana(113) {
            if n.effects.shield.0 == 0 {
                n.effects.shield = Saturating(6);
                nexts.push(n);
            }
        }

        // poison
        if let Some(mut n) = s.try_spend_mana(173) {
            if n.effects.poison.0 == 0 {
                n.effects.poison = Saturating(6);
                nexts.push(n);
            }
        }

        // recharge
        if let Some(mut n) = s.try_spend_mana(229) {
            if n.effects.recharge.0 == 0 {
                n.effects.recharge = Saturating(5);
                nexts.push(n);
            }
        }

        // boss turn
        // update effects
        for n in nexts.iter_mut() {
            // eprintln!("  after player move: {:?}", n);
            apply_turn_start_effects(n);
            // eprintln!("  after boss turn start: {:?}", n);

            if n.boss.hp.0 == 0 {
                // boss doesn't get a turn now
                // eprintln!("  boss dies");
                continue;
            }

            // eprintln!("  boss attacks");
            // boss attacks
            let dmg = n.boss.dmg - n.player.armor as u16;
            n.player.hp -= dmg.max(1);
        }

        for n in &nexts {
            // eprintln!("  n: {:?}", n);
        }

        nexts
    };

    let result = search.run_single(get_next_candidates, is_target_state);

    dbg!(result);

    let part1 = "";
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
