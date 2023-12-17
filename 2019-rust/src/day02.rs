use crate::util::*;
use crate::intcode::*;
use color_eyre::eyre::Result;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 2)?;

    let intcode: IntCode = input.parse()?;

    let mut answer = 0;
    'outer: for noun in 0..100 {
        for verb in 0..100 {
            let result = run_with_inputs(&intcode, noun, verb)?;
            if result == 19690720 {
                answer = 100 * noun + verb;
                break 'outer;
            }
        }
    }

    print!("{}", answer);

    Ok(())
}

fn run_with_inputs(intcode: &IntCode, noun: TInt, verb: TInt) -> Result<TInt> {
    // need a fresh copy of the machine, since running it mutates the memory
    let mut intcode = intcode.clone();

    intcode.set_value_at_position(1, noun);
    intcode.set_value_at_position(2, verb);

    intcode.run()?;

    Ok(intcode.get_value_at(0))
}

#[test]
fn test1() -> Result<()> {
    let mut ic1 = "1,0,0,0,99".parse::<IntCode>()?;
    ic1.run()?;
    assert_eq!(ic1.get_value_at(0), 2);

    let mut ic2 = "1,1,1,4,99,5,6,0,99".parse::<IntCode>()?;
    ic2.run()?;
    assert_eq!(ic2.get_value_at(0), 30);

    Ok(())
}
