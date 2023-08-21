use std::collections::HashSet;
use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;

fn read_input() -> Vec<String> {
    let mut input = Vec::new();
    let path = Path::new("inputs/day03.txt");
    let file = File::open(path).expect("cannot open file");
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        let line = line.expect("cannot read line");
        input.push(line);
    }
    input
}

fn find_dupe(left: &str, right: &str) -> char {
    let left_set: HashSet<char> = left.chars().collect();
    let right_set: HashSet<char> = right.chars().collect();
    *left_set.intersection(&right_set).next().unwrap()
}

fn priority(c: char) -> u8 {
    match c {
        'a'..='z' => (c as u8) - ('a' as u8) + 1,
        'A'..='Z' => (c as u8) - ('A' as u8) + 27,
        _ => panic!("unexpected item: {c}"),
    }
}

fn part1(input: &Vec<String>) -> i32 {
    let mut total_priority: i32 = 0;
    for line in input {
        let (left, right) = line.split_at(line.len() / 2);
        total_priority += priority(find_dupe(left, right)) as i32;
    }
    total_priority
}

fn find_badge(group: &[String]) -> char {
    let common_items: HashSet<char> = group
        .iter()
        .map(|items| items.chars().collect())
        .reduce(|l: HashSet<char>, r: HashSet<char>| l.intersection(&r).map(|item| *item).collect())
        .unwrap();
    *common_items.iter().next().unwrap()
}

fn part2(input: &Vec<String>) -> i32 {
    input
        .chunks(3)
        .map(|group| priority(find_badge(group)) as i32)
        .sum()
}

pub fn solve() {
    let input = read_input();
    println!(
        "DAY 03\n======\npart 1: {}\npart 2: {}\n",
        part1(&input),
        part2(&input)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_dupe() {
        assert_eq!(find_dupe(&"vJrwpWtwJgWr", &"hcsFMMfFFhFp"), 'p');
    }

    #[test]
    fn test_priorities() {
        assert_eq!(priority('a'), 1);
        assert_eq!(priority('b'), 2);
        assert_eq!(priority('A'), 27);
    }

    #[test]
    fn test_find_badge() {
        assert_eq!(
            find_badge(&[
                "vJrwpWtwJgWrhcsFMMfFFhFp".into(),
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL".into(),
                "PmmdzqPrVvPwwTWBwg".into()
            ]),
            'r'
        );
    }
}
