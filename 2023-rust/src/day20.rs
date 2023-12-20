use std::collections::{HashMap, VecDeque, hash_map::{OccupiedEntry, Entry}};

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 20)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let lines = input.trim().lines();
    let nodes = lines.map(|l| l.split_once(" -> ").unwrap());
    let mut modules: HashMap<_, _> = nodes
        .map(|(n, t)| {
            let targets = t.split(",").map(|x| x.trim().to_owned()).collect_vec();
            if n == "broadcaster" {
                (n.to_owned(), Module::Broadcaster(targets))
            } else if let Some(name) = n.strip_prefix('%') {
                (name.to_owned(), Module::Flipflop(false, targets))
            } else if let Some(name) = n.strip_prefix('&') {
                (
                    name.to_owned(),
                    Module::Conjunction(HashMap::new(), targets),
                )
            } else {
                panic!("can't parse {} {}", n, t);
            }
        })
        .collect();

    modules.insert("output".to_owned(), Module::Output);

    let mut target_links = vec![];
    for (source_name, source_module) in modules.iter() {
        for target_name in get_targets(source_module) {
            target_links.push((source_name.clone(), target_name.clone()));
        }
    }

    for (source_name, target_name) in target_links {
        if let Entry::Occupied(mut target_module) = modules.entry(target_name.clone()) {
            if let Module::Conjunction(ref mut inputs, _) = target_module.get_mut() {
                inputs.insert(source_name.clone(), false);
            }

        } else {
            // TODO: there appear to be some target modules that don't appear in the input
            // not sure if there's anything we need to do to handle those?
            // panic!("couldn't find module with name {}", target_name.clone());
        }
    }

    dbg!(&modules);

    let mut total_signals_sent: usize = 0;
    let mut low_signals_sent: usize = 0;
    let mut high_signals_sent: usize = 0;

    let mut track_signal = |s| {
        if s {
            high_signals_sent += 1;
        } else {
            low_signals_sent += 1;
        }
        total_signals_sent += 1;
    };
    for _ in 0..1000 {
        // pushing the button sends a low signal to broadcaster
        track_signal(false);
        let mut queue = VecDeque::new();
        let broadcaster = &modules["broadcaster"];
        let Module::Broadcaster(broadcast_targets) = broadcaster else {
            panic!("broadcaster not a broadcaster")
        };
        for bt in broadcast_targets {
            queue.push_back(("broadcaster".to_owned(), bt.clone(), false));
        }

        while let Some((source, dest, sig_is_high)) = queue.pop_front() {
            track_signal(sig_is_high);
            // println!("handling signal {} for {}", sig_is_high, dest);
            let target_module = modules
                .entry(dest.clone())
                // TODO: missing modules?
                .or_insert(Module::Output);

            match target_module {
                Module::Output => {},
                Module::Broadcaster(_) => panic!("???"),
                Module::Flipflop(ref mut ff_is_on, ff_targets) => {
                    if sig_is_high {
                        // nothing happens
                    } else {
                        *ff_is_on = !*ff_is_on;
                        for t in ff_targets {
                            queue.push_back((dest.clone(), t.clone(), *ff_is_on));
                        }
                    }
                }
                Module::Conjunction(ref mut inputs, c_targets) => {
                    inputs.insert(source.clone(), sig_is_high);

                    // dbg!(&inputs);

                    let output_is_high = if inputs.values().all(|i| *i) {
                        false
                    } else {
                        true
                    };

                    for t in c_targets {
                        queue.push_back((dest.clone(), t.clone(), output_is_high));
                    }
                }
            }
        }
    }

    let part1 = (low_signals_sent * high_signals_sent);
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn get_targets(module: &Module) -> Vec<String> {
    match module {
        Module::Output => vec![],
        Module::Broadcaster(t) => t.clone(),
        Module::Flipflop(_, t) => t.clone(),
        Module::Conjunction(_, t) => t.clone(),
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Module {
    Output,
    // targets
    Broadcaster(Vec<String>),
    // is_on, targets
    Flipflop(bool, Vec<String>),
    // for each input: are we remembering high?, targets
    Conjunction(HashMap<String, bool>, Vec<String>),
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 32000000 | Part 2: ", result);
    Ok(())
}
#[test]
fn test_example2() -> Result<()> {
    let input = r###"
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 11687500 | Part 2: ", result);
    Ok(())
}
