use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use std::collections::{hash_map::Entry, HashMap, VecDeque};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 20)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for<'input>(input: &'input str) -> Result<String> {
    let lines = input.trim().lines();
    let nodes = lines.map(|l| l.split_once(" -> ").unwrap());
    let mut modules: HashMap<&'input str, _> = nodes
        .map(|(n, t)| {
            let targets = t.split(',').map(|x| x.trim()).collect_vec();
            if n == "broadcaster" {
                (n, Module::Broadcaster(targets))
            } else if let Some(name) = n.strip_prefix('%') {
                (name, Module::Flipflop(false, targets))
            } else if let Some(name) = n.strip_prefix('&') {
                (name, Module::Conjunction(HashMap::new(), targets))
            } else {
                panic!("can't parse {} {}", n, t);
            }
        })
        .collect();

    modules.insert(&"output", Module::Output);

    let mut target_links: Vec<(&'input str, &'input str)> = vec![];
    for (source_name, source_module) in modules.iter() {
        for target_name in get_targets(source_module) {
            target_links.push((source_name, target_name));
        }
    }

    for (source_name, target_name) in target_links {
        if let Entry::Occupied(mut target_module) = modules.entry(target_name) {
            if let Module::Conjunction(ref mut inputs, _) = target_module.get_mut() {
                inputs.insert(&source_name, false);
            }
        } else {
            // turns out we might send signals to an undefined module (this is intended)
        }
    }

    let mut total_signals_sent: usize = 0;
    let mut low_signals_sent: usize = 0;
    let mut high_signals_sent: usize = 0;
    let mut button_presses: usize = 0;

    let mut track_signal = |s| {
        if s {
            high_signals_sent += 1;
        } else {
            low_signals_sent += 1;
        }
        total_signals_sent += 1;
    };
    'outer: for _ in 0..4 {
        // pushing the button sends a low signal to broadcaster
        track_signal(false);
        button_presses += 1;
        let mut queue: VecDeque<(&'input str, &'input str, bool)> = VecDeque::new();
        let broadcaster = &modules[&"broadcaster"];
        let Module::Broadcaster(broadcast_targets) = broadcaster else {
            panic!("broadcaster not a broadcaster")
        };
        for bt in broadcast_targets {
            queue.push_back(("broadcaster", bt, false));
        }

        while let Some((source, dest, sig_is_high)) = queue.pop_front() {
            track_signal(sig_is_high);
            // println!("handling signal {} for {}", sig_is_high, dest);
            if dest == "rx" {
                if button_presses % 100_000 == 0 {
                    println!("rx {} [{}]", sig_is_high, button_presses);
                }

                if !sig_is_high {
                    // this takes a while to hit
                    break 'outer;
                }
            }
            let target_module = modules.get_mut(dest);

            match target_module {
                Some(Module::Broadcaster(_)) => panic!("???"),
                Some(Module::Flipflop(ref mut ff_is_on, ff_targets)) => {
                    if sig_is_high {
                        // nothing happens
                    } else {
                        *ff_is_on = !*ff_is_on;
                        for t in ff_targets {
                            queue.push_back((dest, t, *ff_is_on));
                        }
                    }
                }
                Some(Module::Conjunction(ref mut inputs, c_targets)) => {
                    inputs.insert(&source, sig_is_high);

                    if dest == "cn" && inputs.values().any(|i| *i) {
                        println!("cn: bp {} inputs {:?}", button_presses, inputs);
                    }

                    let output_is_high = !inputs.values().all(|i| *i);
                    for t in c_targets {
                        queue.push_back((dest, t, output_is_high));
                    }
                }
                _ => {}
            }
        }
    }

    let part1 = low_signals_sent * high_signals_sent;
    let part2 = "";

    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn get_targets<'input>(module: &Module<'input>) -> Vec<&'input str> {
    match module {
        Module::Output => vec![],
        Module::Broadcaster(t) => t.clone(),
        Module::Flipflop(_, t) => t.clone(),
        Module::Conjunction(_, t) => t.clone(),
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Module<'input> {
    Output,
    // targets
    Broadcaster(Vec<&'input str>),
    // is_on, targets
    Flipflop(bool, Vec<&'input str>),
    // for each input: are we remembering high?, targets
    Conjunction(HashMap<&'input str, bool>, Vec<&'input str>),
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

    assert_eq!("Part 1: 512 | Part 2: ", result);
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

    assert_eq!("Part 1: 187 | Part 2: ", result);
    Ok(())
}
