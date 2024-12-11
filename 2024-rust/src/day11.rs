use std::collections::HashMap;

pub fn blink_memoized_seq<T: std::hash::BuildHasher>(
    cache: &mut HashMap<(usize, u64), usize, T>,
    sequence: &[u64],
    target_num_steps: usize,
) -> usize {
    let mut result = 0;
    for &stone in sequence {
        result += blink_memoized_stone(cache, stone, target_num_steps);
    }
    result
}

pub fn blink_memoized_stone<T: std::hash::BuildHasher>(
    cache: &mut HashMap<(usize, u64), usize, T>,
    stone: u64,
    target_num_steps: usize,
) -> usize {
    if target_num_steps == 0 {
        return 1;
    }
    if let Some(&entry) = cache.get(&(target_num_steps, stone)) {
        entry
    } else {
        let sequence = blink(stone);
        let resulting_length = sequence
            .into_iter()
            .map(|s| blink_memoized_stone(cache, s, target_num_steps - 1))
            .sum();
        cache.insert((target_num_steps, stone), resulting_length);
        resulting_length
    }
}

pub fn blink(stone: u64) -> Vec<u64> {
    if stone == 0 {
        vec![1]
    } else if stone.ilog10() % 2 == 1 {
        // even number of digits
        let str = stone.to_string();
        let left = &str[0..str.len() / 2];
        let right = &str[str.len() / 2..str.len()];
        return vec![left.parse().unwrap(), right.parse().unwrap()];
    } else {
        return vec![stone * 2024];
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use color_eyre::Result;
    use itertools::Itertools;

    #[test]
    fn test_example1() -> Result<()> {
        let sequence = vec![0, 1, 10, 99, 999];

        let result = sequence.into_iter().map(blink).collect_vec();

        assert_eq!(
            result,
            vec![vec![1], vec![2024], vec![1, 0], vec![9, 9], vec![2021976]]
        );
        Ok(())
    }
}
