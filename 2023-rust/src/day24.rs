use crate::utils::*;
use color_eyre::eyre::Result;
use itertools::Itertools;

pub fn solve() -> Result<()> {
    let input = get_input(2023, 24)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let inputs = input
        .trim()
        .lines()
        .map(|l| all_numbers_f64(l))
        .map(|ns| Hail {
            x0: ns[0],
            y0: ns[1],
            z0: ns[2],
            dx: ns[3],
            dy: ns[4],
            dz: ns[5],
        })
        .collect_vec();

    dbg!(&inputs);

    let mut collisions:usize = 0;

    for i in 0..inputs.len() {
        for j in (i + 1)..inputs.len() {
            let ha = &inputs[i];
            let hb = &inputs[j];

            // consider all distinct pairs of hailstones:
            println!("Comparing a={:?} and b={:?}", ha, hb);

            /*
             for part 1
             a hailstone moves in a line determined by

             x = x0 + t*dx
             y = y0 + t*dy

             where t is a time parameter

             the hailstones don't have to collide exactly - we just
             have to find out if their paths intersect.

             (checking this working using https://gamedev.stackexchange.com/a/44733/114877 )
                                  
             the path of ha is defined by x_a = x0_a + t_a * dx_a (and same for y)
             the path of hb is defined by x_b = x0_b + t_b * dx_b (and same for y)

             since they don't have to collide exactly, t_a and t_b don't
             have to be the same value.

             however if there is an intersection, then we have x_a = x_b, so 

             x0_a + t_a * dx_a = x0_b + t_b * dx_b

             let's try to isolate t_a first:

             t_a * dx_a = x0_b + t_b * dx_b
             
             t_a = (x0_b + t_b * dx_b) / dx_a
             and same for y:
             t_a = (y0_b + t_b * dy_b) / dy_a

             this t_a is the same for both equations, though, so now we have

             (x0_b + t_b * dx_b) / dx_a = (y0_b + t_b * dy_b) / dy_a

             which we should be able to use to isolate t_b

             let's try multiplying by dy_a first:

             (x0_b + t_b * dx_b) * (dy_a / dx_a) = y0_b + t_b * dy_b

             x0_b * (dy_a / dx_a) + (t_b * dx_b) * (dy_a / dx_a) = y0_b + t_b * dy_b

             (t_b * dx_b) * (dy_a / dx_a) = y0_b + t_b * dy_b - (x0_b * (dy_a / dx_a))
             (t_b * dx_b) * (dy_a / dx_a) - (t_b * dy_b) = y0_b - (x0_b * (dy_a / dx_a))

             t_b * ((dx_b * dy_a / dx_a) - dy_b) = y0_b - (x0_b * (dy_a / dx_a))

             t_b = (y0_b - (x0_b * (dy_a / dx_a))) / (((dx_b * dy_a / dx_a) - dy_b))


            */

            let t_b = (hb.y0 - (hb.x0 * (ha.dy / ha.dx))) / (((hb.dx * ha.dy / ha.dx) - hb.dy));
            let t_a = (hb.x0 + t_b * hb.dx) / ha.dx;

            println!("t_b {:.3} t_a {:.3}", t_b, t_a);
            if t_a.is_infinite() || t_b.is_infinite() {
                println!("nonintersecting");
                continue;
            }

            let x_a = ha.x0 + (ha.dx * t_a);
            let y_a = ha.y0 + (ha.dy * t_a);
            let x_b = hb.x0 + (hb.dx * t_b);
            let y_b = hb.y0 + (hb.dy * t_b);

            println!("a: {:.3} {:.3} b: {:.3} {:.3}", x_a, y_a, x_b, y_b);
        }
    }



    let part1 = collisions;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

struct Hail {
    x0: f64,
    y0: f64,
    z0: f64,
    dx: f64,
    dy: f64,
    dz: f64,
}

impl std::fmt::Debug for Hail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("H({},{},{} | {} {} {})", self.x0, self.y0, self.z0, self.dx, self.dy, self.dz))
    }
}

#[test]
fn test_example1() -> Result<()> {
    let input = r###"
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"###;
    let result = solve_for(input)?;

    assert_eq!("Part 1: 2 | Part 2: ", result);
    Ok(())
}
