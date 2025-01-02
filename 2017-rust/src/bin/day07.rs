use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 7)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (&str, u32) {
    let mut part2 = 0;

    let mut parents = FxHashMap::default();
    let mut children = FxHashMap::default();
    let mut weights = FxHashMap::<&str, u32>::default();
    for line in input.trim().lines() {
        if let Some((left, right)) = line.split_once(" -> ") {
            let above = right.split(", ");
            let (below, weight) = left.split_once(" ").unwrap();
            let weight = all_numbers(weight)[0];
            weights.insert(below, weight);
            for a in above.clone() {
                parents.insert(a, below);
            }
            children.insert(below, above.collect_vec());
        } else {
            let (below, weight) = line.split_once(" ").unwrap();
            let weight = all_numbers(weight)[0];
            weights.insert(below, weight);
        }
    }

    let mut root = weights.keys().next().unwrap();
    let mut root_parent = parents.get(root);
    while root_parent.is_some() {
        root = root_parent.unwrap();
        root_parent = parents.get(root);
    }

    let mut summed_weights = FxHashMap::default();
    get_summed_weights(root, &weights, &children, &mut summed_weights);
    // eprintln!("{:?}", summed_weights);

    let mut problem = *root;
    let mut next_problem = is_unbalanced(&children, root, &summed_weights);
    while next_problem.is_some() {
        problem = next_problem.unwrap();
        next_problem = is_unbalanced(&children, problem, &summed_weights);
    }

    eprintln!("{:?}", problem);

    let problem_parent = *&parents[problem];
    let problem_parent_children = &children[problem_parent];
    let nonproblem_child = problem_parent_children
        .iter()
        .find(|&&c| c != problem)
        .unwrap();
    let target_weight = summed_weights[nonproblem_child];
    let correct_child_weight =
        summed_weights[children[problem][0]] * children[problem].len() as u32;
    dbg!(
        problem_parent,
        problem_parent_children,
        nonproblem_child,
        target_weight,
        correct_child_weight
    );
    part2 = target_weight - correct_child_weight;

    (root, part2)
}

fn is_unbalanced<'i>(
    children: &FxHashMap<&'i str, Vec<&'i str>>,
    root: &'i str,
    summed_weights: &FxHashMap<&'i str, u32>,
) -> Option<&'i str> {
    let mut bad_proc = root;
    let mut bad_chld = &children[bad_proc];
    let weight_counts = bad_chld.iter().map(|c| summed_weights[c]).counts();
    if weight_counts.len() != 1 {
        eprintln!("{}'s children are unbalanced", bad_proc);
        let odd_weight = weight_counts
            .iter()
            .find(|(_, &c)| c == 1)
            .map(|(x, _)| x)
            .unwrap();

        let odd_proc = bad_chld
            .iter()
            .map(|c| (c, summed_weights[c]))
            .find(|x| x.1 == *odd_weight)
            .unwrap();
        return Some(odd_proc.0);
    } else {
        eprintln!("{}'s children are balanced", bad_proc);
        return None;
    }
}

fn get_summed_weights<'i>(
    start: &'i str,
    weights: &FxHashMap<&'i str, u32>,
    children: &FxHashMap<&'i str, Vec<&'i str>>,
    result: &mut FxHashMap<&'i str, u32>,
) {
    // eprintln!("{:?}", start);
    let this_weight = *&weights[start];
    match children.get(start) {
        Some(chld) => {
            chld.iter()
                .for_each(|c| get_summed_weights(c, weights, children, result));
            result.insert(
                start,
                this_weight + chld.iter().map(|c| result[c]).sum::<u32>(),
            );
        }
        None => {
            result.insert(start, this_weight);
        }
    }
}

#[test]
fn test_example1() {
    let input = r###"
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, "tknk");
    assert_eq!(part2, 60);
}
