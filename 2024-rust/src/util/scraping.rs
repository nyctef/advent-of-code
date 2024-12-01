use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref DIGITS: Regex = Regex::new(r"\d+").unwrap();
    static ref NEGATIVE_DIGITS: Regex = Regex::new(r"[-\d]+").unwrap();
}

// TODO: try making this generic? (or use a macro?)

pub fn all_numbers(input: &str) -> Vec<u32> {
    DIGITS
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}

pub fn all_numbers_usize(input: &str) -> Vec<usize> {
    DIGITS
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}

pub fn all_numbers_isize(input: &str) -> Vec<isize> {
    NEGATIVE_DIGITS
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

pub fn all_numbers_i64(input: &str) -> Vec<i64> {
    NEGATIVE_DIGITS
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}
pub fn all_numbers_f64(input: &str) -> Vec<f64> {
    NEGATIVE_DIGITS
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}
