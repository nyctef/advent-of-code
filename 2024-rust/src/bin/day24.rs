use aoc_2024_rust::util::*;
use color_eyre::eyre::Result;
use itertools::Itertools;
use rustc_hash::FxHashMap;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    let input = get_input(2024, 24)?;

    let (part1, part2) = solve_for(&input);

    println!("Part 1: {} | Part 2: {}", part1, part2);
    Ok(())
}

fn solve_for(input: &str) -> (u64, u64) {
    let (inputs, gates) = input.trim().split_once("\n\n").unwrap();
    let inputs = inputs.lines().map(|l| {
        let (wire, value) = l.split_once(": ").unwrap();
        let value = if value == "0" { false } else { true };
        (wire, value)
    });
    let inputs = FxHashMap::from_iter(inputs);

    let gates = gates.lines().map(|l| {
        let (input, output) = l.split_once(" -> ").unwrap();
        let (l, op, r) = input.splitn(3, " ").collect_tuple().unwrap();

        (output, (l, op, r))
    });
    let gates = FxHashMap::from_iter(gates);

    // dbg!(&gates.iter().sorted().collect_vec());

    for i in 0..45 {
        let mut log = vec![];
        let n = 2_u64.pow(i);
        let result = run_circuit(n, n, &gates, &mut log);
        if result != n * 2 {
            eprintln!();
            eprintln!("adding {} and {} failed: expected\n{:#045b} but got\n{:#045b} instead", n, n, n*2, result);
            // for log_line in log {
            //     eprintln!("{}", log_line);
            // }
        }
    }

    let mut part1 = 0;
    let mut part2 = 0;

    (part1, part2)
}

fn run_circuit(
    mut x: u64,
    mut y: u64,
    gates: &FxHashMap<&str, (&str, &str, &str)>,
    log: &mut Vec<String>,
) -> u64 {
    // dbg!(&inputs, &gates);
    //
    let mut values = FxHashMap::default();
    for i in 0..45 {
        let x_val = x & 1;
        x >>= 1;
        values.entry(format!("x{:02}", i)).insert_entry(x_val);
        let y_val = y & 1;
        y >>= 1;
        values.entry(format!("y{:02}", i)).insert_entry(y_val);
    }

    // dbg!(&values);
    // dbg!(&values.iter().sorted().collect_vec());

    'outer: loop {
        for (wire, gate) in gates {
            let &(l, op, r) = gate;

            if !values.contains_key(*wire) && values.contains_key(l) && values.contains_key(r) {
                let l_value = values[l];
                let r_value = values[r];
                let new_value = match op {
                    "AND" => l_value & r_value,
                    "OR" => l_value | r_value,
                    "XOR" => l_value ^ r_value,
                    _ => panic!("unknown operation {}", op),
                };

                log.push(format!(
                    "{} ({}) {} {} ({}) -> {} ({})",
                    l, l_value, op, r, r_value, wire, new_value
                ));

                values.entry(wire.to_string()).insert_entry(new_value);

                continue 'outer;
            }
        }

        break;
    }

    let z_values = values
        .iter()
        .filter(|(k, v)| k.starts_with('z'))
        .sorted()
        .map(|(k, v)| v);
    // dbg!(&z_values);

    let mut part1 = 0;
    let mut p2 = 0;
    for z in z_values {
        part1 += z * 2_u64.pow(p2);
        p2 += 1;
    }
    part1
}

#[test]
fn test_example1() {
    let input = r###"
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
"###;
    let (part1, part2) = solve_for(input);

    assert_eq!(part1, 2024);
    assert_eq!(part2, 0);
}
