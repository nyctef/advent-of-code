use crate::utils::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use std::collections::{HashMap, VecDeque};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 19)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let (workflows, _items) = input.trim().split_once("\n\n").unwrap();
    // let items = items.trim().lines().map(|l| all_numbers(l)).collect_vec();
    let workflows: HashMap<_, _> = workflows
        .trim()
        .lines()
        .map(|l| {
            let (name, rest) = l.split_once('{').unwrap();
            let rest = rest.strip_suffix('}').unwrap();
            let conditions = rest
                .split(',')
                .map(|c| {
                    if let Some((cond, dest)) = c.split_once(':') {
                        let prop = match cond.chars().next().unwrap() {
                            'x' => 0,
                            'm' => 1,
                            'a' => 2,
                            's' => 3,
                            _ => panic!("xmas"),
                        };
                        let gtlt = cond.chars().nth(1).unwrap() == '>';
                        let dest = dest.to_string();
                        let value: u32 = cond[2..].parse().unwrap();

                        Condition::GTLT(prop, gtlt, value, dest)
                    } else {
                        let dest = c.to_string();
                        Condition::Always(dest)
                    }
                })
                .collect_vec();
            (name, conditions)
        })
        .collect();

    let mut accepted = vec![];
    let starting_beam: Beam = (
        RangeInc::new(1, 4000),
        RangeInc::new(1, 4000),
        RangeInc::new(1, 4000),
        RangeInc::new(1, 4000),
    );

    let mut q = VecDeque::new();
    q.push_front(("in".to_string(), starting_beam));

    while let Some((flow, beam)) = q.pop_front() {
        let workflow = &workflows[&flow.as_str()];

        let mut remaining_beam = beam;
        for work in workflow {
            let (split, remaining) = split_beam(&remaining_beam, &work);
            if let Some((dest, split)) = split {
                if dest == "A" {
                    accepted.push(split);
                } else if dest == "R" {
                    // forget about it
                } else {
                    // queue the work for another workflow
                    q.push_front((dest, split));
                }

            }

            if let Some(remaining) = remaining {
                remaining_beam = remaining;
                continue;
            } else {
                break;
            }

        }

    }

    /*
    for item in items {
        let mut current_work = &workflows["in"];
        'workflow: loop {
            for work in current_work {
                match work {
                    Condition::Always(dest) => {
                        if dest == "A" {
                            accepted.push(item);
                            break 'workflow;
                        } else if dest == "R" {
                            break 'workflow;
                        } else {
                            current_work = &workflows[dest.as_str()];
                            continue 'workflow;
                        }
                    }
                    Condition::GTLT(p, gt, v, dest) => {
                        let rating = item[*p];
                        if (*gt && rating > *v) || (!*gt && rating < *v) {
                            if dest == "A" {
                                accepted.push(item);
                                break 'workflow;
                            } else if dest == "R" {
                                break 'workflow;
                            } else {
                                current_work = &workflows[dest.as_str()];
                                continue 'workflow;
                            }
                        }
                    }
                }
            }
            panic!("ran out of work in this workflow");
        }
    }
    */

    //let part1 = accepted.iter().map(|a| a.iter().sum::<u32>()).sum::<u32>();
    let part2 = "";
    Ok(format!("Part 2: {part2}"))
}

type Beam = (RangeInc, RangeInc, RangeInc, RangeInc);

// returns split, remaining
fn split_beam(beam: &Beam, condition: &Condition) -> (Option<(String, Beam)>, Option<Beam>) {
    todo!()
}

#[derive(Debug, Eq, PartialEq, Constructor)]
struct RangeInc {
    start: u32,
    end: u32,
}

#[derive(Debug, Eq, PartialEq)]
enum Condition {
    // destination
    Always(String),
    // property, is_gt, value to compare against, destination
    GTLT(usize, bool, u32, String),
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 2: 167409079868000", result);
    Ok(())
}
