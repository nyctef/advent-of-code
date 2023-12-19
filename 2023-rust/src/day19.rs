use std::collections::HashMap;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 19)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let (workflows, items) = input.trim().split_once("\n\n").unwrap();
    let items = items.trim().lines().map(|l| all_numbers(l)).collect_vec();
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

    let part1 = accepted.iter().map(|a| a.iter().sum::<u32>()).sum::<u32>();
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
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

    assert_eq!("Part 1: 19114 | Part 2: ", result);
    Ok(())
}
