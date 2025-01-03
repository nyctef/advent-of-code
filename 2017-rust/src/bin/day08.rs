use aoc_2017_rust::util::*;
use color_eyre::eyre::Result;
use derive_more::Constructor;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2017, 8)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (i64, u64) {
    let mut part1 = 0;
    let mut part2 = 0;

    let instructions = input
        .trim()
        .lines()
        .map(|l| {
            let parts = l.split(' ');
            eprintln!("{:?}", parts.clone().collect_vec());
            assert!(parts.clone().count() == 7);

            let part = |i: usize| parts.clone().nth(i).unwrap();

            let amount = part(2).parse::<i64>().unwrap();
            let op = match part(1) {
                "inc" => Operation::Inc(amount),
                "dec" => Operation::Dec(amount),
                x => panic!("unrecognised operation {x}"),
            };

            let boundary = part(6).parse::<i64>().unwrap();
            let cond = match part(5) {
                "==" => Condition::Eq(boundary),
                "!=" => Condition::Ne(boundary),
                ">" => Condition::Gt(boundary),
                ">=" => Condition::Ge(boundary),
                "<=" => Condition::Le(boundary),
                "<" => Condition::Lt(boundary),
                x => panic!("unrecognised condition {x}"),
            };

            Instruction::new(part(0), part(4), op, cond)
        })
        .collect_vec();

    let mut registers = FxHashMap::default();

    for instr in instructions {
        let other: i64 = *registers.entry(instr.other).or_default();
        if match instr.cond {
            Condition::Gt(x) => other > x,
            Condition::Ge(x) => other >= x,
            Condition::Eq(x) => other == x,
            Condition::Ne(x) => other != x,
            Condition::Lt(x) => other < x,
            Condition::Le(x) => other <= x,
        } {
            let reg = registers.entry(instr.reg).or_default();
            match instr.op {
                Operation::Inc(x) => *reg += x,
                Operation::Dec(x) => *reg -= x,
            }
        }
    }

    part1 = *registers.values().max().unwrap();

    (part1, part2)
}

#[derive(Debug, Constructor)]
struct Instruction<'i> {
    reg: &'i str,
    other: &'i str,
    op: Operation,
    cond: Condition,
}

#[derive(Debug)]
enum Operation {
    Inc(i64),
    Dec(i64),
}

#[derive(Debug)]
enum Condition {
    Gt(i64),
    Ge(i64),
    Eq(i64),
    Ne(i64),
    Lt(i64),
    Le(i64),
}

#[test]
fn test_example1() {
    let input = r###"
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 1);
    assert_eq!(part2, 0);
}
