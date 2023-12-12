use crate::utils::*;
use color_eyre::eyre::Result;
#[allow(unused_imports)]
use itertools::{intersperse, repeat_n, Itertools};
use std::collections::VecDeque;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 12)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let mut total = 0;
    for line in input.trim().lines() {
        // let line = &unfold(line);
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
    for (space, spring) in st.space_choices.iter().zip(spec.iter()) {
        add_n_chars(&mut result, '.', *space as usize);
        add_n_chars(&mut result, '#', *spring as usize);
    }
    result
}

fn solve_line(line: &str) -> u32 {
    let mut total = 0;
    let (pattern, line_spec) = line.split_once(" ").unwrap();
    let spec = all_numbers(line_spec);
    for candidate in generate_candidates(&spec, pattern.len() as u32) {
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

fn generate_candidates(spec: &[u32], target_length: u32) -> Vec<String> {
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
    // println!("generate_choices sa{} ta{}", spaces_available, targets_available);
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
    // println!("choices: {}", &result.len());
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
fn test_example2() {
    assert_eq!(solve_line(&unfold("???.### 1,1,3")), 1);
    assert_eq!(solve_line(&unfold(".??..??...?##. 1,1,3")), 16384);
}

// #[test]
// fn test_generate_candidates() {
//     assert_eq!(generate_candidates("1,1,3", 7), vec!["#.#.###"]);
//     assert_eq!(
//         generate_candidates("1,1,3", 8),
//         vec![".#.#.###", "#..#.###", "#.#..###", "#.#.###.",]
//     );
// }

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

fn unfold(input: &str) -> String {
    let (pattern, line_spec) = input.split_once(" ").unwrap();
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
