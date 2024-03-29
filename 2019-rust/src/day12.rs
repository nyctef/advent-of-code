use crate::util::*;
use color_eyre::eyre::Result;
use color_eyre::Report;
use itertools::chain;
use itertools::Itertools;
use nom::bytes::complete::is_a;
use nom::bytes::complete::tag;
use nom::character;
use nom::multi::separated_list1;
use nom::sequence::tuple;
use nom::Finish;
use nom::IResult;
use std::collections::HashMap;
use std::fmt::Display;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 12)?;

    // have to map_err to get an owned copy of &input that can outlive this function,
    // since the error message may refer to parts of &input
    let (_, mut muns) = parse_muns(&input)
        .finish()
        .map_err(|err| Report::msg(err.to_string()))?;

    // println!("round 0");
    // println!("{}", muns.iter().map(|x| x.to_string()).join("\n"));
    // println!();
    let mut seen_xs_cycle = false;
    let mut seen_ys_cycle = false;
    let mut seen_zs_cycle = false;
    let mut seen_positions: HashMap<Vec<(i32, i32)>, i32> = HashMap::new();
    for round in 0..100_000_000 {
        if round % 10_000_000 == 0 {
            dbg!(round, seen_positions.len());
        }
        simulate(&mut muns);

        let xs = muns.iter().map(|m| (m.pos_x, m.vel_x)).collect_vec();
        let ys = muns.iter().map(|m| (m.pos_y, m.vel_y)).collect_vec();
        let zs = muns.iter().map(|m| (m.pos_z, m.vel_z)).collect_vec();

        check_cycle(&mut seen_positions, xs, &mut seen_xs_cycle, round, "xs");
        check_cycle(&mut seen_positions, ys, &mut seen_ys_cycle, round, "ys");
        check_cycle(&mut seen_positions, zs, &mut seen_zs_cycle, round, "zs");
    }

    Ok(())
}

fn check_cycle(
    seen_positions: &mut HashMap<Vec<(i32, i32)>, i32>,
    xs: Vec<(i32, i32)>,
    seen_this_cycle: &mut bool,
    round: i32,
    cycle_name: &str,
) {
    let seen_xs = seen_positions.entry(xs.clone());
    match seen_xs {
        std::collections::hash_map::Entry::Occupied(prev_round) => {
            if !*seen_this_cycle {
                println!(
                    "At round {} : seen position {:?} already at round {:?}",
                    round,
                    cycle_name,
                    &prev_round.get()
                );
                *seen_this_cycle = true;
            }
        }
        std::collections::hash_map::Entry::Vacant(_) => {
            seen_positions.insert(xs, round);
        }
    }
}

fn simulate(muns: &mut [Mun]) {
    for m1 in 0..muns.len() {
        let (lower_muns, rest) = muns.split_at_mut(m1);
        let (this_mun, higher_muns) = rest.split_at_mut(1);

        assert!(this_mun.len() == 1);
        assert!(lower_muns.len() + higher_muns.len() == 3);
        let this_mun = &mut this_mun[0];

        #[allow(clippy::comparison_chain)]
        for other_mun in chain!(lower_muns.iter(), higher_muns.iter()) {
            if other_mun.pos_x > this_mun.pos_x {
                this_mun.vel_x += 1
            } else if other_mun.pos_x < this_mun.pos_x {
                this_mun.vel_x -= 1
            }

            if other_mun.pos_y > this_mun.pos_y {
                this_mun.vel_y += 1
            } else if other_mun.pos_y < this_mun.pos_y {
                this_mun.vel_y -= 1
            }

            if other_mun.pos_z > this_mun.pos_z {
                this_mun.vel_z += 1
            } else if other_mun.pos_z < this_mun.pos_z {
                this_mun.vel_z -= 1
            }
        }
    }

    for mun in muns.iter_mut() {
        mun.pos_x += mun.vel_x;
        mun.pos_y += mun.vel_y;
        mun.pos_z += mun.vel_z;
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct Mun {
    pos_x: i32,
    pos_y: i32,
    pos_z: i32,
    vel_x: i32,
    vel_y: i32,
    vel_z: i32,
}
impl Mun {
    fn new(x: i32, y: i32, z: i32) -> Mun {
        Mun {
            pos_x: x,
            pos_y: y,
            pos_z: z,
            vel_x: 0,
            vel_y: 0,
            vel_z: 0,
        }
    }
}
impl Display for Mun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "pos=<x={:>3} y={:>3} z={:>3}> vel=<x={:>3} y={:>3} z={:>3}>",
            self.pos_x, self.pos_y, self.pos_z, self.vel_x, self.vel_y, self.vel_z
        )
    }
}

fn parse_muns(input: &str) -> IResult<&str, Vec<Mun>> {
    let (rest, muns) = separated_list1(tag("\n"), parse_mun)(input)?;
    Ok((rest, muns))
}

fn parse_mun(input: &str) -> IResult<&str, Mun> {
    let (rest, (_, attrs, _)) =
        tuple((tag("<"), separated_list1(tag(", "), parse_attr), tag(">")))(input)?;

    let x = attrs.iter().find(|a| a.0 == "x").unwrap().1;
    let y = attrs.iter().find(|a| a.0 == "y").unwrap().1;
    let z = attrs.iter().find(|a| a.0 == "z").unwrap().1;

    Ok((rest, Mun::new(x, y, z)))
}

fn parse_attr(input: &str) -> IResult<&str, (&str, i32)> {
    let (rest, name) = is_a("xyz")(input)?;
    let (rest, _) = tag("=")(rest)?;
    let (rest, value) = character::complete::i32(rest)?;

    Ok((rest, (name, value)))
}

#[test]
fn test1() {
    // ...
}
