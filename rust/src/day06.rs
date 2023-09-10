use std::collections::HashSet;
use crate::common;

fn part1(input: &str) -> usize {
    find_marker(&input, 4)
}

fn part2(input: &str) -> usize {
    find_marker(&input, 14)
}

fn find_marker(input: &str, len: usize) -> usize {
    for i in len..input.len() {
        let window = &input[(i - len)..i];
        let set: HashSet<char> = window.chars().collect();
        if set.len() == len {
            return i;
        }
    }
    panic!("marker not found");
}

pub fn solve() {
    let lines = common::slurp_input(6);
    let input = lines.first().unwrap();
    println!(
        "DAY 06\n======\npart 1: {}\npart 2: {}\n",
        part1(input),
        part2(input)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7);
        assert_eq!(part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19);
        assert_eq!(part2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23);
    }
}
