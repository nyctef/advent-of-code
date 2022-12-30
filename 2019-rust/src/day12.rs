use nom::bytes::complete::is_a;
use nom::bytes::complete::tag;
use nom::character;
use nom::multi::separated_list1;
use nom::sequence::tuple;
use nom::IResult;

use crate::aoc_util::*;
use crate::err_util::*;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 12)?;

    // TODO: how to make ? work here?
    let (_, muns) = parse_muns(&input).unwrap();
    dbg!(muns);

    Ok(())
}

#[derive(Debug)]
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
