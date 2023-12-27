use crate::util::*;
use color_eyre::eyre::Result;
use regex::Regex;
use std::{collections::HashMap, str::FromStr};

pub fn solve() -> Result<()> {
    let input = get_input(2019, 22)?;

    let result = solve_for(&input, 2020, 119315717514047, 101741582076661)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str, target: isize, size: isize, iterations: usize) -> Result<String> {
    let pos = solve_hyper(input, target, size, iterations);

    Ok(format!(
        "final number on card at position {}: {}",
        target, pos
    ))
}

fn solve_hyper(input: &str, target: isize, size: isize, iterations: usize) -> isize {
    let start_sequence = solve_brute_force(input, target, size, 3);
    let fac1 = get_linear_factors(
        start_sequence[0],
        start_sequence[1],
        start_sequence[1],
        start_sequence[2],
        size,
    );
    let mut factors = HashMap::new();
    factors.insert(1, fac1);
    println!("fac1 {:?}", fac1);

    let mut speed = 1;

    let mut pos = target;
    let mut iteration = 0;
    while iteration < iterations {
        println!("i {} I-i {} s {}", iteration, iterations - iteration, speed);

        while iteration + (speed * 2) < iterations {
            println!("s: {} trying to speed up", speed);
            let prev_factors = factors[&speed];
            speed *= 2;
            let seq = solve_from_factors(prev_factors, target, size, 5);
            let new_factors = get_linear_factors(seq[0], seq[2], seq[2], seq[4], size);
            factors.insert(speed, new_factors);
        }

        while iteration + speed > iterations {
            println!("s: {} need to slow down", speed);
            speed /= 2;
        }

        let next = solve_from_factors(factors[&speed], pos, size, 2);
        pos = next[1];
        println!("fs: {:?} next_pos: {}", factors[&speed], pos);

        iteration += speed;
    }
    println!("final i: {} pos: {}", iteration, pos);
    pos
}

fn solve_from_factors(fac: (isize, isize), start: isize, size: isize, count: usize) -> Vec<isize> {
    let mut pos = start;
    let mut result = vec![start];
    while result.len() < count {
        pos = modmul(pos, fac.0, size);
        pos = (pos + fac.1).rem_euclid(size);
        result.push(pos);
    }
    result
}

fn solve_brute_force(input: &str, start: isize, size: isize, count: usize) -> Vec<isize> {
    let mut pos = start;
    let re = Regex::from_str(r"-?\d+").unwrap();
    let mut result = vec![start];
    while result.len() < count {
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
        result.push(pos);
    }
    result
}

fn get_linear_factors(a: isize, b: isize, c: isize, d: isize, m: isize) -> (isize, isize) {
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
    // println!("a {} b {} c {} d {}", a, b, c, d,);
    let cia = modmul(c, modmulinv(a, m), m);
    let y = modmul(modmulinv(1 - cia, m), d - modmul(cia, b, m), m);
    let x = modmul(modmulinv(a, m), b - y, m);

    (x, y)
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
    let (gcd, mut a, _) = extended_euclidean_algorithm(a, b);
    assert!(
        gcd.abs() == 1,
        "gcd({},{}) needs to be 1 but got {}",
        a,
        b,
        gcd
    ); // otherwise the modular inverse isn't properly defined
    if gcd < 0 {
        a = -a;
    }
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
    let at_7 = solve_brute_force(&input, 7, 10, 2);
    let at_7 = at_7[1];
    assert_eq!(0, at_7);
    let at_8 = solve_brute_force(&input, 8, 10, 2);
    let at_8 = at_8[1];
    assert_eq!(3, at_8);
    Ok(())
}

#[test]
fn test_hyper_against_brute_force() {
    let input = get_input(2019, 22).unwrap();
    let from_brute_force = solve_brute_force(&input, 2020, 119315717514047, 100);
    for i in 0..100 {
        println!();
        println!("testing i={i}");
        let faster = solve_hyper(&input, 2020, 119315717514047, i);
        assert_eq!(faster, from_brute_force[i]);
    }
}
