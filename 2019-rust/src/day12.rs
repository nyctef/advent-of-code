use nom::bytes::complete::is_a;
use nom::bytes::complete::tag;
use nom::character;
use nom::IResult;

use crate::aoc_util::*;
use crate::err_util::*;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 12)?;

    let muns = parse_muns(&input);
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
    let (rest, _) = tag("<")(input)?;

    let (rest, attrs) = nom::multi::separated_list1(tag(", "), parse_attr)(rest)?;

    let (rest, _) = tag(">")(rest)?;

    println!("attrs");
    dbg!(attrs);

    Ok((rest, vec![]))
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
