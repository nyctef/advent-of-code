pub fn all_numbers(input: &str) -> Vec<u32> {
    regex::Regex::new(r"\d+")
        .unwrap()
        .find_iter(input)
        .map(|x| x.as_str().parse().unwrap())
        .collect()
}
