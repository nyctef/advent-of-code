use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref DIGITS: Regex = Regex::new(r"\d+").unwrap();
}

pub fn all_numbers(input: &str) -> Vec<u32> {
    DIGITS
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}

pub fn all_numbers_u64(input: &str) -> Vec<u64> {
    DIGITS
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}
