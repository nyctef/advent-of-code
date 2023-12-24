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
        .map(|l| all_numbers_i64(l))
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
        for j in 0..i {
            let h1 = &inputs[i];
            let h2 = &inputs[j];

            // consider all distinct pairs of hailstones:
            println!("Comparing {:?} and {:?}", h1, h2);

            /*
             for part 1
             a hailstone moves in a line determined by

             x = x0 + t*dx
             y = y0 + t*dy

             if two hailstones a and b intersect, then we have

             x0a + t*dxa = x0b + t*dxb
             y0a + t*dya = y0b + t*dyb

             we want to find the value of t at the intersection, if it exists
             since we need to know if t is in the past (negative)

             t*dxa - t*dxb = x0b - x0a

             t(dxa - dxb) = x0b - x0a

             t = (x0b - x0a) / (dxa - dxb)
             t = (y0b - y0a) / (dya - dyb)


            */

            // if (h1.dx, h1.dy) == (h2.dx, h2.dy) {
            //     println!("{:?} and {:?} are parallel", h1, h2);
            //     continue;
            // }
            //
            let tx: f64 = (h2.x0 - h1.x0) as f64 / (h1.dx - h2.dx) as f64;
            let ty: f64 = (h2.y0 - h1.y0) as f64 / (h1.dy - h2.dy) as f64;

            let x1 = h1.x0 as f64 + tx*h1.dx as f64;
            let y1 = h1.y0 as f64 + ty*h1.dy as f64;
            let x2 = h2.x0 as f64 + tx*h2.dx as f64;
            let y2 = h2.y0 as f64 + ty*h2.dy as f64;

            println!("tx{} ty{} | x1{} y1{} | x2{} y2{}", tx, ty, x1, y1, x2, y2);
        }
    }



    let part1 = collisions;
    let part2 = "";
    Ok(format!("Part 1: {part1} | Part 2: {part2}"))
}

#[derive(Eq, PartialEq)]
struct Hail {
    x0: i64,
    y0: i64,
    z0: i64,
    dx: i64,
    dy: i64,
    dz: i64,
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
