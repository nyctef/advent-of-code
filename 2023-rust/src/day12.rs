use crate::utils::*;
use color_eyre::eyre::Result;
#[allow(unused_imports)]
use itertools::{intersperse, repeat_n, Itertools};
use std::{
    collections::{HashMap, HashSet},
    iter,
};

pub fn solve() -> Result<()> {
    let input = get_input(2023, 12)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut total = 0;
    for line in input.trim().lines() {
        let line = &unfold(line);
        let line_score = solve_line(line);
        println!("{} -> {}", line, line_score);
        total += line_score;
    }
    let part1 = total;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct SearchState {
    space_choices: Vec<u32>,
}

impl SearchState {
    fn and(&self, n: u32) -> SearchState {
        let mut res = self.clone();
        res.space_choices.push(n);
        res
    }
}

fn generate_partial_candidate(spec: &[u32], st: &SearchState) -> String {
    let mut result = String::new();
    let mut first = true;
    for (space, spring) in st
        .space_choices
        .iter()
        .zip(spec.iter().chain(iter::repeat(&0)))
    {
        add_n_chars(&mut result, '.', *space as usize);
        if !first {
            add_n_chars(&mut result, '.', 1);
        }
        add_n_chars(&mut result, '#', *spring as usize);
        first = false;
    }
    result
}

fn solve_line(line: &str) -> u64 {
    let mut total: u64 = 0;
    let (pattern, line_spec) = line.split_once(' ').unwrap();
    println!("p: {} ls: {}", &pattern, &line_spec);
    let spec = all_numbers(line_spec);
    let mut solution_cache: HashMap<(String, Vec<u32>), u64> = HashMap::new();
    total += solve_spec(pattern, spec, false, &mut solution_cache);

    // for candidate in generate_candidates(&spec, pattern.len() as u32) {
    //     if candidate_matches_pattern(&candidate, pattern) {
    //         total += 1;
    //     }
    // }
    total
}

fn solve_spec(
    pattern: &str,
    spec: Vec<u32>,
    space_at_beginning_required: bool,
    solution_cache: &mut HashMap<(String, Vec<u32>), u64>,
) -> u64 {
    let mut total: u64 = 0;
    let mut seen: HashSet<SearchState> = HashSet::new();

    if spec.is_empty() {
        return if pattern.chars().all(|c| c == '.' || c == '?') {
            1
        } else {
            0
        };
    }

    let target_length = pattern.len() as u32;
    let minimum_spaces = (spec.len() - 1) as u32;
    let min_candidate_length: u32 = spec.iter().sum::<u32>() + minimum_spaces;
    let free_spaces: i32 = target_length as i32 - min_candidate_length as i32;
    let space_positions = spec.len() + 1;

    let mut counter: u64 = 0;

    counter += 1;
    if counter % 100_000 == 0 {
        // println!("count: {} next: {:?}", counter, next);
    }
    // let start = if next.space_choices.is_empty() { 0 } else { 1 };
    let next = SearchState {
        space_choices: vec![],
    };

    let consumed_so_far = next.space_choices.iter().sum::<u32>();
    let choices_made_so_far = next.space_choices.len();
    if choices_made_so_far + 1 > space_positions {
        return total;
    }
    let spaces_remaining = free_spaces - consumed_so_far as i32;

    // dbg!(spaces_remaining);

    let start = if space_at_beginning_required { 1 } else { 0 };
    for i in start..=spaces_remaining {
        let n2 = next.and(i as u32);

        if seen.contains(&n2) {
            continue;
        }
        seen.insert(n2.clone());

        let candidate = generate_partial_candidate(&spec, &n2);
        if candidate.len() > pattern.len() {
            continue;
        }
        let matches = candidate_matches_pattern(&candidate, pattern);
        /*
        if matches {
            println!(
                "c: {} | n2: {} | m: {}",
                candidate,
                n2.space_choices.iter().map(|x| format!("{}", x)).join(","),
                matches
            );
        }
        */
        // dbg!(&spec, &n2, &candidate);
        if matches {
            if candidate.len() == pattern.len() {
                // println!("FULL MATCH {}", candidate);
                total += 1;
            } else {
                // p: ?### c: ### -> can't allow a fourth # to match, so don't skip a char
                // p: .??. c: .#  -> can't allow a # at the third char, so offset by one before
                //                   continuing
                let next_space_at_beginning_required =
                    candidate.len() < pattern.len() && candidate.ends_with('#');
                let offset = candidate.len();
                let new_pattern = pattern[offset..].to_string();
                let new_spec = spec.iter().skip(1).copied().collect_vec();
                // println!("recursing to {} {:?}", &new_pattern, &new_spec);
                let k = (new_pattern.clone(), new_spec.clone());
                let cached = solution_cache.get(&k);
                if let Some(st) = cached {
                    // println!("t: {} | got {} cached subtotal", total, st);
                    total += st;
                } else {
                    let subtotal = solve_spec(
                        &new_pattern,
                        new_spec,
                        next_space_at_beginning_required,
                        solution_cache,
                    );
                    // println!("t: {} | got {} calculated subtotal", total, subtotal);
                    solution_cache.insert(k, subtotal);
                    total += subtotal;
                }
            }
        }
    }

    // println!("returning t: {}", total);
    total
}

fn candidate_matches_pattern(candidate: &str, pattern: &str) -> bool {
    for (c, p) in candidate.chars().zip(pattern.chars()) {
        if c == p {
            continue;
        }
        if p == '?' {
            continue;
        }
        return false;
    }
    true
}

fn add_n_chars(target: &mut String, chr: char, n: usize) {
    // todo: is there a builtin for this?
    for _ in 0..n {
        target.push(chr);
    }
}

#[test]
fn test_example1() {
    assert_eq!(solve_line("#.#.### 1,1,3"), 1);
    assert_eq!(solve_line("???.### 1,1,3"), 1);
    assert_eq!(solve_line("?###? 3"), 1);
    assert_eq!(solve_line(".??..??...?##. 1,1,3"), 4);
    assert_eq!(solve_line("?###???????? 3,2,1"), 10);
}

#[test]
fn test_example2() {
    assert_eq!(solve_line(&unfold("???.### 1,1,3")), 1);
    assert_eq!(solve_line(&unfold(".??..??...?##. 1,1,3")), 16384);
}

#[test]
fn test_candidate_matches_pattern() {
    assert!(candidate_matches_pattern("#", "#"));
    assert!(!candidate_matches_pattern("#", "."));
    assert!(candidate_matches_pattern("#", "?"));
    assert!(candidate_matches_pattern(".", "?"));
    assert!(candidate_matches_pattern(".", "."));
    assert!(!candidate_matches_pattern(".", "#"));
    assert!(candidate_matches_pattern("###", "###"));
    assert!(!candidate_matches_pattern("###", "#.#"));
    assert!(candidate_matches_pattern("###", "#?#"));
}

fn unfold(input: &str) -> String {
    let (pattern, line_spec) = input.split_once(' ').unwrap();
    let mut result = String::new();
    for x in intersperse(repeat_n(pattern, 5), "?") {
        result.push_str(x);
    }
    result.push(' ');
    for x in intersperse(repeat_n(line_spec, 5), ",") {
        result.push_str(x);
    }
    result
}

#[test]
fn test_unfold() {
    assert_eq!(unfold(".# 1"), ".#?.#?.#?.#?.# 1,1,1,1,1");
}
