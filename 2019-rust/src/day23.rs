use std::{
    collections::{HashMap, VecDeque},
    str::FromStr,
};

use crate::{intcode::IntCode, util::*};
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 23)?;

    let result = solve_for(&input)?;

    println!("{}", result);
    Ok(())
}

fn solve_for(input: &str) -> Result<String> {
    let intcode = IntCode::from_str(input)?;

    let mut network = vec![];

    for i in 0..50 {
        let mut comp = intcode.clone();
        comp.queue_input(i);
        network.push(comp);
    }

    let mut network_queue = Vec::new();
    for _ in 0..50 {
        network_queue.push(VecDeque::new());
    }
    let mut natx = 0;
    let mut naty = 0;
    let mut prev_naty = 0;
    loop {
        // push packets to computers
        for i in 0..50 {
            let q = &mut network_queue[i];
            let c = &mut network[i];
            if q.is_empty() {
                c.queue_input(-1);
            }

            while let Some(v) = q.pop_front() {
                c.queue_input(v);
            }
        }

        // run computers
        for i in 0..50 {
            network[i].run()?;
        }

        // read sent packets into the queue
        for i in 0..50 {
            let c = &mut network[i];
            while let Some(i) = c.read_output() {
                let x = c.read_output().expect("x");
                let y = c.read_output().expect("y");

                if i == 255 {
                    natx = x;
                    naty = y;
                    println!("nat got new packet {x} {y}");
                } else {
                    network_queue[i as usize].push_back(x);
                    network_queue[i as usize].push_back(y);
                }
            }
        }

        if network_queue.iter().all(|q| q.len() == 0) {
            println!("nat sending packet {natx} {naty}");
            if naty == prev_naty {
                return Ok(format!("result: {}", naty));
            }
            network_queue[0].push_back(natx);
            network_queue[0].push_back(naty);
            prev_naty = naty;
        }
    }

}
