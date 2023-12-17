use crate::util::*;
use crate::intcode::*;
use color_eyre::eyre::Result;
#[cfg(test)]
use color_eyre::Report;

pub fn solve() -> Result<()> {
    let input = get_input(2019, 5)?;

    let mut intcode = input.parse::<IntCode>()?;
    // intcode.print_memory();
    intcode.queue_input(5);
    // println!("running...");
    let result = intcode.run();
    // println!("completed");
    // intcode.print_memory();
    while let Some(o) = intcode.read_output() {
        dbg!(o);
    }

    result
}

#[test]
fn test_example() -> Result<()> {
    fn example(value: TInt) -> Result<TInt> {
        let code = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99";
        let mut intcode = code.parse::<IntCode>()?;
        intcode.print_memory();
        intcode.queue_input(value);
        intcode.run()?;

        intcode
            .read_output()
            .ok_or_else(|| Report::msg("failed to produce output"))
    }

    assert_eq!(999, example(5)?);
    assert_eq!(1000, example(8)?);
    assert_eq!(1001, example(10)?);

    Ok(())
}

#[test]
fn test_simple() -> Result<()> {
    fn eq_8(value: TInt) -> Result<TInt> {
        let mut intcode = "3,9,8,9,10,9,4,9,99,-1,8".parse::<IntCode>()?;
        intcode.queue_input(value);
        intcode.run()?;

        intcode
            .read_output()
            .ok_or_else(|| Report::msg("failed to produce output"))
    }
    assert_eq!(1, eq_8(8)?);
    assert_eq!(0, eq_8(9)?);

    Ok(())
}
