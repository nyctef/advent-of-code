use std::collections::VecDeque;

use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 12)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut total = 0;
    for line in input.trim().lines() {
        total += solve_line(line);
    }
    let part1 = total;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

fn solve_line(line: &str) -> u32 {
    let mut total = 0;
    let (pattern, line_spec) = line.split_once(" ").unwrap();
    for candidate in generate_candidates(line_spec, pattern.len() as u32) {
        if candidate_matches_pattern(&candidate, pattern) {
            total += 1;
        }
    }
    total
}

fn candidate_matches_pattern(candidate: &str, pattern: &str) -> bool {
    assert!(candidate.len() == pattern.len());

    for (c, p) in candidate.chars().zip(pattern.chars()) {
        if c == p {
            continue;
        }
        if p == '?' {
            continue;
        }
        return false;
    }
    return true;
}

fn generate_candidates(line_spec: &str, target_length: u32) -> Vec<String> {
    let spec = all_numbers(line_spec);
    let minimum_spaces = (spec.len() - 1) as u32;
    let min_candidate_length: u32 = spec.iter().sum::<u32>() + minimum_spaces;
    let free_spaces: i32 = target_length as i32 - min_candidate_length as i32;
    let space_positions = spec.len() + 1;
    assert!(free_spaces >= 0);

    let mut result = Vec::new();

    for space_choice in generate_choices(free_spaces as usize, space_positions) {
        let mut candidate = String::new();
        // dbg!(&space_choice, &spec);
        let mut choices = space_choice.into_iter();
        add_n_chars(&mut candidate, '.', choices.next().unwrap());
        for (i, spec_item) in spec.iter().enumerate() {
            add_n_chars(&mut candidate, '#', *spec_item as usize);
            if i != spec.len() - 1 {
                add_n_chars(&mut candidate, '.', 1);
            }
            add_n_chars(&mut candidate, '.', choices.next().unwrap());
        }

        result.push(candidate);
    }
    result
}

fn add_n_chars(target: &mut String, chr: char, n: usize) {
    // todo: is there a builtin for this?
    for _ in 0..n {
        target.push(chr);
    }
}

fn generate_choices(spaces_available: usize, targets_available: usize) -> Vec<Vec<usize>> {
    if targets_available <= 0 {
        return vec![];
    }

    let mut result = vec![];
    let mut q: VecDeque<Vec<usize>> = VecDeque::new();
    q.push_front(vec![]);
    while let Some(next) = q.pop_front() {
        let consumed_so_far = next.iter().sum::<usize>();
        let choices_made_so_far = next.len();
        let choices_remaining = targets_available - choices_made_so_far;
        let spaces_remaining = spaces_available - consumed_so_far;
        // println!();
        // println!("csf {} cmsf {} cr {} sr {}", consumed_so_far, choices_made_so_far, choices_remaining, spaces_remaining);

        if choices_made_so_far == targets_available {
            // we must have consumed all spaces by now
            // dbg!(consumed_so_far, spaces_available);
            assert!(consumed_so_far == spaces_available);
            result.push(next);
            continue;
        }

        if choices_remaining == 1 {
            let mut next2 = next.clone();
            next2.push(spaces_remaining);
            q.push_front(next2);
            continue;
        }

        for next_choice in 0..=spaces_remaining {
            let mut next2 = next.clone();
            next2.push(next_choice);
            q.push_front(next2);
        }
    }
    result
}

#[test]
fn test_example1() {
    assert_eq!(solve_line("#.#.### 1,1,3"), 1);
    assert_eq!(solve_line("???.### 1,1,3"), 1);
    assert_eq!(solve_line(".??..??...?##. 1,1,3"), 4);
    assert_eq!(solve_line("?###???????? 3,2,1"), 10);
}

#[test]
fn test_generate_candidates() {
    assert_eq!(generate_candidates("1,1,3", 7), vec!["#.#.###"]);
    assert_eq!(
        generate_candidates("1,1,3", 8),
        vec![".#.#.###", "#..#.###", "#.#..###", "#.#.###.",]
    );
}

#[test]
fn test_generate_choices() {
    // one item, and three places to put it
    assert_eq!(
        generate_choices(1, 3),
        vec![vec![1, 0, 0], vec![0, 1, 0], vec![0, 0, 1],]
    );

    // two items, and two places to put them
    assert_eq!(
        generate_choices(2, 2),
        vec![vec![2, 0], vec![1, 1], vec![0, 2],]
    );

    // three items, and one place to put them
    assert_eq!(generate_choices(3, 1), vec![vec![3]]);
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
