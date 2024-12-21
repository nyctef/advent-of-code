use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::iter;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 21)?;

    let part1 = solve_for(&input, 2)?;
    let part2 = solve_for(&input, 25)?;

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str, nesting_level: usize) -> Result<usize> {
    let codes = input.trim().lines().collect_vec();

    let keypad = CharGrid::from_string(
        r###"
789
456
123
.0A
"###,
    );
    let keypad_start = keypad.find_single_char('A');
    let directions = CharGrid::from_string(
        r###"
.^A
<v>
"###,
    );
    let directions_start = directions.find_single_char('A');

    let mut part1 = 0;

    let mut cost_per_level = FxHashMap::default();
    let dirs = vec!['A', '^', '<', 'v', '>'];
    for depth in (0..=nesting_level).rev() {
        for &start in &dirs {
            for &end in &dirs {
                if depth == nesting_level {
                    cost_per_level.insert((start, end, depth), 1);
                } else {
                    let possible_expansions =
                        track_moves(directions.find_single_char(start), &vec![end], &directions);
                    let possible_expansions = expand_moves(possible_expansions);

                    let get_expansion_cost = |expansion: &Vec<char>| {
                        let mut current = 'A';
                        let mut total = 0;
                        for i in 0..expansion.len() {
                            total += cost_per_level
                                .get(&(current, expansion[i], depth + 1))
                                .unwrap();
                            current = expansion[i];
                        }
                        total
                    };

                    let min_expansion_cost = possible_expansions
                        .iter()
                        .map(get_expansion_cost)
                        .min()
                        .unwrap();
                    cost_per_level.insert((start, end, depth), min_expansion_cost);
                }
            }
        }
    }

    // dbg!(&cost_per_level);

    for code in codes {
        eprintln!("code: {}", code);
        let first_robot_moves = track_moves(keypad_start, &code.chars().collect_vec(), &keypad);
        let first_robot_moves = expand_moves(first_robot_moves);

        let mut lowest_len = usize::MAX;
        for candidate in first_robot_moves {
            let mut current = 'A';
            let mut total = 0;
            for m in candidate {
                total += cost_per_level
                    .get(&(current, m, 0))
                    .unwrap();
                current = m;
            }

            lowest_len = lowest_len.min(total);
        }

        let complexity = lowest_len * code.trim_matches('A').parse::<usize>().unwrap();
        eprintln!("complexity: {}", complexity);
        part1 += complexity;
    }

    Ok(part1)
}

fn expand_moves(forest: Vec<Vec<Vec<char>>>) -> Vec<Vec<char>> {
    // input: a list of possible choices of sequences for each char in the target code
    // output: a list of full sequences using each combination of those choices
    //
    //
    forest
        .into_iter()
        .multi_cartesian_product()
        .map(|x| x.into_iter().flatten().collect_vec())
        .collect_vec()
}

#[test]
fn test_expand_moves() {
    let moves_1 = vec![
        vec![vec!['a', 'b'], vec!['c', 'd']],
        vec![vec!['1', '2'], vec!['3', '4']],
    ];

    assert_eq!(
        expand_moves(moves_1),
        [
            vec!['a', 'b', '1', '2'],
            vec!['a', 'b', '3', '4'],
            vec!['c', 'd', '1', '2'],
            vec!['c', 'd', '3', '4']
        ]
    );
}

fn track_moves(start: CharGridIndexRC, target: &[char], map: &CharGrid) -> Vec<Vec<Vec<char>>> {
    let mut robot_pos = start;
    let mut choices_per_t = vec![vec![vec![]]];
    let mut panic_pos = map.find_single_char('.');
    let mut prev_direction = RCDirection::left();
    for &t in target {
        let target_pos = map.find_single_char(t);
        let dist = RCDirection::from_to(robot_pos, target_pos);

        // assume that it's always going to be fastest to do a full move in one direction followed
        // by moving as far as possible in the other (otherwise the outer robot would spend too
        // much time switching between buttons)
        //
        // we need to avoid the `.` since the robot panics on that square
        // if we're on the same row as the `.` then move vertically first
        //
        // otherwise we want to stay on the same button as we were previously if possible
        //
        // best transitions:
        // same button (left->left, right->right etc)
        // good transitions: (up<->down, left<->down, right<->down)
        // two moves: (left<->right, left<->up, right<->up)

        let move_horizontally = |result: &mut Vec<char>, robot_pos: &mut CharGridIndexRC| {
            if dist.coldiff < 0 {
                result.extend(iter::repeat_n('<', dist.coldiff.abs() as usize));
            } else if dist.coldiff > 0 {
                result.extend(iter::repeat_n('>', dist.coldiff.abs() as usize));
            }
            robot_pos.col = (robot_pos.col as isize + dist.coldiff) as usize;
        };
        let move_vertically = |result: &mut Vec<char>, robot_pos: &mut CharGridIndexRC| {
            if dist.rowdiff < 0 {
                result.extend(iter::repeat_n('^', dist.rowdiff.abs() as usize));
            } else if dist.rowdiff > 0 {
                result.extend(iter::repeat_n('v', dist.rowdiff.abs() as usize));
            }
            robot_pos.row = (robot_pos.row as isize + dist.rowdiff) as usize;
        };

        let mut choices_for_this_t = vec![];

        let mut choice_1 = vec![];
        let mut choice_2 = vec![];
        let mut choice_1_has_panicked = false;
        let mut choice_2_has_panicked = false;
        {
            let mut robot_1_pos = robot_pos;
            move_vertically(&mut choice_1, &mut robot_1_pos);
            if robot_1_pos == panic_pos {
                choice_1_has_panicked = true;
            }
            move_horizontally(&mut choice_1, &mut robot_1_pos);
            if robot_1_pos == panic_pos {
                choice_1_has_panicked = true;
            }

            assert!(robot_1_pos == target_pos);
            choice_1.push('A');
        }
        {
            let mut robot_2_pos = robot_pos;
            move_horizontally(&mut choice_2, &mut robot_2_pos);
            if robot_2_pos == panic_pos {
                choice_2_has_panicked = true;
            }
            move_vertically(&mut choice_2, &mut robot_2_pos);
            if robot_2_pos == panic_pos {
                choice_2_has_panicked = true;
            }
            assert!(robot_2_pos == target_pos);
            choice_2.push('A');
        }

        if choice_1.eq(&choice_2) {
            // no choice (purely vertical or horizontal move?)
            choices_for_this_t.push(choice_1);
        } else {
            if !choice_1_has_panicked {
                choices_for_this_t.push(choice_1);
            }
            if !choice_2_has_panicked {
                choices_for_this_t.push(choice_2);
            }
        }

        robot_pos = target_pos;
        choices_per_t.push(choices_for_this_t);
    }
    choices_per_t
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
029A
980A
179A
456A
379A
"###;
    let part1 = solve_for(input, 2)?;

    assert_eq!(part1, 126384);
    Ok(())
}
