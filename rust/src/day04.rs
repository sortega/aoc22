use std::fs::File;
use std::io;
use std::io::BufRead;
use std::ops::RangeInclusive;
use std::path::Path;

type Section = i32;
type SectionRange = RangeInclusive<Section>;
type Input = Vec<(SectionRange, SectionRange)>;

fn read_input() -> Input {
    let mut input: Input = Vec::new();
    let path = Path::new("inputs/day04.txt");
    let file = File::open(path).expect("cannot open file");
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        let line = line.expect("cannot read line");
        let (from1, to1, from2, to2) =
            scan_fmt!(&line, "{}-{},{}-{}", i32, i32, i32, i32).expect("cannot parse line");
        input.push((from1..=to1, from2..=to2));
    }
    input
}

fn part1(input: &Input) -> i32 {
    input
        .iter()
        .filter(|pair| is_inside(&pair.0, &pair.1) || is_inside(&pair.1, &pair.0))
        .count() as i32
}

fn part2(input: &Input) -> i32 {
    input
        .iter()
        .filter(|pair| overlaps(&pair.0, &pair.1))
        .count() as i32
}

fn is_inside<T: PartialOrd>(inner: &RangeInclusive<T>, outer: &RangeInclusive<T>) -> bool {
    inner.start() >= outer.start() && inner.end() <= outer.end()
}

fn overlaps(left: &RangeInclusive<i32>, right: &RangeInclusive<i32>) -> bool {
    let start = std::cmp::max(left.start(), right.start());
    let end = std::cmp::min(left.end(), right.end());
    start <= end
}

pub fn solve() {
    let input = read_input();
    println!(
        "DAY 04\n======\npart 1: {}\npart 2: {}\n",
        part1(&input),
        part2(&input)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_inside() {
        assert!(is_inside(&(1..=3), &(1..=10)));
        assert!(!is_inside(&(1..=10), &(1..=3)));
    }

    #[test]
    fn test_overlap() {
        assert!(overlaps(&(1..=3), &(1..=10)));
        assert!(overlaps(&(1..=10), &(1..=3)));
    }
}
