pub fn all_numbers(input: &str) -> Vec<u32> {
    regex::Regex::new(r"\d+")
        .unwrap()
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}

pub fn all_numbers_u64(input: &str) -> Vec<u64> {
    regex::Regex::new(r"\d+")
        .unwrap()
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}
