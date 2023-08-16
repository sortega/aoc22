use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;

fn read_input() -> Vec<(char, char)> {
    let mut input = Vec::new();
    let path = Path::new("inputs/day02.txt");
    let file = File::open(path).expect("cannot open file");
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        let line = line.expect("cannot read line");
        input.push((line.chars().nth(0).unwrap(), line.chars().nth(2).unwrap()));
    }
    input
}

#[derive(PartialEq, Debug, Clone)]
enum Outcome {
    Lose,
    Draw,
    Win,
}

impl Outcome {
    fn score(&self) -> i32 {
        match *self {
            Outcome::Lose => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        }
    }

    fn parse(c: char) -> Outcome {
        match c {
            'X' => Outcome::Lose,
            'Y' => Outcome::Draw,
            'Z' => Outcome::Win,
            _ => panic!("cannot parse {c} into an outcome"),
        }
    }

    fn strategy_to_get(&self, their_strategy: Strategy) -> Strategy {
        match (self, their_strategy) {
            (Outcome::Draw, _) => their_strategy,
            (Outcome::Win, Strategy::Rock) => Strategy::Paper,
            (Outcome::Win, Strategy::Paper) => Strategy::Scissors,
            (Outcome::Win, Strategy::Scissors) => Strategy::Rock,
            (Outcome::Lose, Strategy::Rock) => Strategy::Scissors,
            (Outcome::Lose, Strategy::Paper) => Strategy::Rock,
            (Outcome::Lose, Strategy::Scissors) => Strategy::Paper,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Strategy {
    Rock,
    Paper,
    Scissors,
}

impl Strategy {
    fn score(&self) -> i32 {
        match *self {
            Strategy::Rock => 1,
            Strategy::Paper => 2,
            Strategy::Scissors => 3,
        }
    }

    fn against(&self, other: Strategy) -> Outcome {
        match (*self, other) {
            (Strategy::Rock, Strategy::Scissors)
            | (Strategy::Paper, Strategy::Rock)
            | (Strategy::Scissors, Strategy::Paper) => Outcome::Win,
            (left, right) if left == right => Outcome::Draw,
            _ => Outcome::Lose,
        }
    }

    fn parse(c: char) -> Strategy {
        match c {
            'A' | 'X' => Strategy::Rock,
            'B' | 'Y' => Strategy::Paper,
            'C' | 'Z' => Strategy::Scissors,
            _ => panic!("cannot parse {c} into a strategy"),
        }
    }
}

fn part1(input: &Vec<(char, char)>) -> i32 {
    let mut total_score = 0;
    for tuple in input {
        let their_strategy = Strategy::parse(tuple.0);
        let my_strategy = Strategy::parse(tuple.1);
        let outcome = my_strategy.against(their_strategy);
        let score = my_strategy.score() + outcome.score();
        total_score += score
    }
    total_score
}

fn part2(input: &Vec<(char, char)>) -> i32 {
    let mut total_score = 0;
    for tuple in input {
        let their_strategy = Strategy::parse(tuple.0);
        let outcome = Outcome::parse(tuple.1);
        let my_strategy = outcome.strategy_to_get(their_strategy);
        let score = my_strategy.score() + outcome.score();
        total_score += score
    }
    total_score
}

pub fn solve() {
    let input = read_input();
    println!(
        "DAY 02\n======\npart 1: {}\npart 2: {}\n",
        part1(&input),
        part2(&input)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strategy() {
        assert_eq!(Strategy::Rock.against(Strategy::Rock), Outcome::Draw);
        assert_eq!(Strategy::Rock.against(Strategy::Paper), Outcome::Lose);
        assert_eq!(Strategy::Rock.against(Strategy::Scissors), Outcome::Win);
        assert_eq!(Strategy::Paper.against(Strategy::Rock), Outcome::Win);
        assert_eq!(Strategy::Paper.against(Strategy::Paper), Outcome::Draw);
        assert_eq!(Strategy::Paper.against(Strategy::Scissors), Outcome::Lose);
        assert_eq!(Strategy::Scissors.against(Strategy::Rock), Outcome::Lose);
        assert_eq!(Strategy::Scissors.against(Strategy::Paper), Outcome::Win);
        assert_eq!(Strategy::Scissors.against(Strategy::Scissors), Outcome::Draw);
    }

    #[test]
    fn test_part1() {
        let input = vec![
            ('A', 'Y'),
            ('B', 'X'),
            ('C', 'Z')
        ];
        assert_eq!(part1(&input), 15);
    }

    #[test]
    fn test_part2() {
        let input = vec![
            ('A', 'Y'),
            ('B', 'X'),
            ('C', 'Z')
        ];
        assert_eq!(part2(&input), 12);
    }
}