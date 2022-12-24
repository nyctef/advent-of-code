mod day1;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let usage = "Usage: [exe] [day] [puzzle|example|input-name]";
    let _day: u8 = std::env::args()
        .nth(1)
        .ok_or(usage)?
        .parse()
        .map_err(|_| usage.to_owned() + ": failed to parse day as u8")?;
    let _path = std::env::args().nth(2).ok_or(usage)?;
    Ok(())
}
