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

                        Condition::Ltgt(prop, gtlt, value, dest)
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
    let starting_beam: Beam = vec![
        RangeInc::new(1, 4000),
        RangeInc::new(1, 4000),
        RangeInc::new(1, 4000),
        RangeInc::new(1, 4000),
    ];

    let mut q = VecDeque::new();
    q.push_front(("in".to_string(), starting_beam));

    while let Some((flow, beam)) = q.pop_front() {
        let workflow = &workflows[&flow.as_str()];

        let mut remaining_beam = beam;
        for work in workflow {
            let (split, remaining) = split_beam(&remaining_beam, work);
            if let Some((dest, split)) = split {
                let size = split.iter().map(|r| r.size()).product::<usize>();
                // output for https://sankeymatic.com/build/
                println!("{flow} [{size}] {dest}");
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

    // for each of the accepted beams, we count the number of possibilities
    // it contains (multiplying the ranges together gives the number of ways
    // you could pick a distinct number from each of the ranges)
    let part2 = accepted
        .iter()
        .map(|a| a.iter().map(|x| x.size()).product::<usize>())
        .sum::<usize>();
    Ok(format!("Part 2: {part2}"))
}

type Beam = Vec<RangeInc>;

// returns split, remaining
fn split_beam(beam: &Beam, condition: &Condition) -> (Option<(String, Beam)>, Option<Beam>) {
    match condition {
        Condition::Always(dest) => (Some((dest.clone(), beam.clone())), None),
        Condition::Ltgt(p, gt, v, dest) => {
            let relevant_range = beam[*p];

            let (matching, notmatching) = split_range(relevant_range, *gt, *v);

            let matching_beam = matching.map(|r| (dest.to_string(), replace_range(beam, r, *p)));
            let remaining_beam = notmatching.map(|r| replace_range(beam, r, *p));

            (matching_beam, remaining_beam)
        }
    }
}

fn replace_range(beam: &Beam, new_range: RangeInc, p: usize) -> Beam {
    let mut new_beam = beam.clone();
    new_beam[p] = new_range;
    new_beam
}

// returns matching, not matching
#[allow(clippy::collapsible_else_if)]
fn split_range(range: RangeInc, gt: bool, v: u32) -> (Option<RangeInc>, Option<RangeInc>) {
    if gt {
        if range.start > v {
            (Some(range), None)
        } else if range.end < v {
            (None, Some(range))
        } else {
            (
                Some(RangeInc::new(v + 1, range.end)),
                Some(RangeInc::new(range.start, v)),
            )
        }
    } else
    /* lt */
    {
        if range.end < v {
            (Some(range), None)
        } else if range.start > v {
            (None, Some(range))
        } else {
            (
                Some(RangeInc::new(range.start, v - 1)),
                Some(RangeInc::new(v, range.end)),
            )
        }
    }
}

#[derive(Debug, Eq, PartialEq, Constructor, Copy, Clone)]
struct RangeInc {
    start: u32,
    end: u32,
}

impl RangeInc {
    fn size(&self) -> usize {
        // since this range is inclusive we need to make sure that [3, 3] counts as size 1
        (self.end + 1) as usize - self.start as usize
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Condition {
    // destination
    Always(String),
    // property, is_gt, value to compare against, destination
    Ltgt(usize, bool, u32, String),
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
