use crate::util::*;
use color_eyre::eyre::Result;
use regex::Regex;
use std::str::FromStr;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 22)?;

    let result = solve_for(&input, 119315717514047, 2020, 101741582076661)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, size: isize, target: isize, iterations: usize) -> Result<String> {
    let mut pos = target;
    let re = Regex::from_str(r"-?\d+")?;

    let mut iteration = 0;
    let mut prevpos = target;
    let mut prevprevpos = target;
    while iteration < iterations {
        // if iteration % 10_000_000 == 0 {
        println!("i {} pos {} ", iteration, pos,);
        // }
        for line in input.trim().lines().rev() {
            // println!("{}", line);
            if line.starts_with("deal into") {
                pos = size - pos - 1;
                continue;
            }
            let num: isize = re.find(line).unwrap().as_str().parse().unwrap();
            if line.starts_with("cut") {
                // pos = (pos - num).rem_euclid(size);
                pos = (pos + num).rem_euclid(size);
            }

            if line.starts_with("deal with") {
                // pos = (pos * num).rem_euclid(size);
                // println!("inv of {} mod {} is {}", num, size, modmulinv(num, size));
                pos = modmul(pos, modmulinv(num, size), size);
            }
        }
        iteration += 1;

        if iteration >= 2 {
            /*
            assuming that the whole sequence of operations collapses down to an ax + y linear
            equation

            then taking two iterations of the sequence:

            ax + y = b  mod m
            cx + y = d  mod m

            ax = b - y  mod m
            x = inv(a) * (b - y)  mod m
            c * (inv(a) * (b - y)) + y = d  mod m

            c*inv(a)*b - c*inv(a)*y + y = d  mod m
            - c*inv(a)*y + y = d - c*inv(a)*b  mod m
            y*(1 - c*inv(a)) = d - c*inv(a)*b  mod m

            y = inv(1-c*inv(a)) * (d - c*inv(a)*b)  mod m
            */
            let a = prevprevpos;
            let b = prevpos;
            let c = prevpos;
            let d = pos;
            println!("a {} b {} c {} d {}", a, b, c, d,);
            let cia = modmul(c, modmulinv(a, size), size);
            let y = modmul(modmulinv(1 - cia, size), d - modmul(cia, b, size), size);
            let x = modmul(modmulinv(a, size), b - y, size);

            println!("x {} y {}", x, y);
        }
        prevprevpos = prevpos;
        prevpos = pos;
        if iteration >= 7 {
            break;
        }
    }

    Ok(format!(
        "final number on card at position {}: {}",
        target, pos
    ))
}

// based on https://github.com/TheAlgorithms/Rust/blob/master/src/math/extended_euclidean_algorithm.rs
// (MIT licensed (c) 2019 "The Algorithms"

fn update_step(a: &mut isize, old_a: &mut isize, quotient: isize) {
    let temp = *a;
    *a = *old_a - quotient * temp;
    *old_a = temp;
}

pub fn extended_euclidean_algorithm(a: isize, b: isize) -> (isize, isize, isize) {
    let (mut old_r, mut rem) = (a, b);
    let (mut old_s, mut coeff_s) = (1, 0);
    let (mut old_t, mut coeff_t) = (0, 1);

    while rem != 0 {
        let quotient = old_r / rem;

        update_step(&mut rem, &mut old_r, quotient);
        update_step(&mut coeff_s, &mut old_s, quotient);
        update_step(&mut coeff_t, &mut old_t, quotient);
    }

    (old_r, old_s, old_t)
}

// TODO: understand this better
pub fn modmulinv(a: isize, b: isize) -> isize {
    let (gcd, a, _) = extended_euclidean_algorithm(a, b);
    assert!(gcd == 1); // otherwise the modular inverse isn't properly defined
    a
}

pub fn modmul(a: isize, b: isize, m: isize) -> isize {
    (a as i128 * b as i128).rem_euclid(m as i128) as isize
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
"###;
    assert_eq!(
        "final number on card at position 7: 0",
        solve_for(input, 10, 7, 1)?
    );
    assert_eq!(
        "final number on card at position 8: 3",
        solve_for(input, 10, 8, 1)?
    );
    Ok(())
}
