use crate::common;

type Crate = char;
type Stack = Vec<Crate>;
type Stacks = Vec<Stack>;
#[derive(PartialEq, Debug)]
struct Command {
    amount: usize,
    from: usize,
    to: usize,
}

impl Command {
    fn move_individually(&self, stacks: &mut Stacks) {
        for _ in 0..self.amount {
            let popped_crate = stacks[self.from - 1].pop().unwrap();
            stacks[self.to - 1].push(popped_crate);
        }
    }

    fn move_in_batches(&self, stacks: &mut Stacks) {
        let new_size = stacks[self.from - 1].len() - self.amount;
        let mut popped_crates = Vec::from(&stacks[self.from - 1][new_size..]);
        stacks[self.from - 1].truncate(new_size);
        stacks[self.to - 1].append(&mut popped_crates);
    }
}

#[derive(PartialEq, Debug)]
struct Input {
    stacks: Stacks,
    commands: Vec<Command>,
}

fn parse_input(input: &Vec<String>) -> Input {
    let line_groups: Vec<&[String]> = input.split(|line| line.is_empty()).take(2).collect();

    Input {
        stacks: parse_stacks(line_groups[0]),
        commands: parse_commands(line_groups[1]),
    }
}

fn parse_stacks(lines: &[String]) -> Stacks {
    let num_stacks = lines
        .last()
        .unwrap()
        .chars()
        .filter(|c| c.is_numeric())
        .count();
    let mut stacks: Stacks = vec![Vec::new(); num_stacks];
    for line in lines.iter().rev().skip(1) {
        for (index, c) in line.chars().enumerate().filter(|&(_, c)| c.is_alphabetic()) {
            let stack_index = (index - 1) / 4;
            stacks[stack_index].push(c);
        }
    }
    stacks
}

fn parse_commands(lines: &[String]) -> Vec<Command> {
    lines
        .iter()
        .map(|line| {
            let (amount, from, to) = scan_fmt!(&line, "move {} from {} to {}", usize, usize, usize)
                .expect("cannot parse line");
            Command { amount, from, to }
        })
        .collect()
}

fn top_crates(stacks: &Stacks) -> String {
    stacks.iter().flat_map(|stack| stack.last()).collect()
}

fn part1(input: &Input) -> String {
    let mut stacks = input.stacks.clone();
    for command in &input.commands {
        command.move_individually(&mut stacks);
    }
    top_crates(&stacks)
}

fn part2(input: &Input) -> String {
    let mut stacks = input.stacks.clone();
    for command in &input.commands {
        command.move_in_batches(&mut stacks);
    }
    top_crates(&stacks)
}

pub fn solve() {
    let lines = common::slurp_lines(5);
    let input = parse_input(&lines);
    println!(
        "DAY 05\n======\npart 1: {}\npart 2: {}\n",
        part1(&input),
        part2(&input)
    );
}

#[cfg(test)]
mod tests {
    use crate::day05::*;

    fn test_input() -> Input {
        Input {
            stacks: vec![vec!['Z', 'N'], vec!['M', 'C', 'D'], vec!['P']],
            commands: vec![
                Command {
                    amount: 1,
                    from: 2,
                    to: 1,
                },
                Command {
                    amount: 3,
                    from: 1,
                    to: 3,
                },
                Command {
                    amount: 2,
                    from: 2,
                    to: 1,
                },
                Command {
                    amount: 1,
                    from: 1,
                    to: 2,
                },
            ],
        }
    }

    #[test]
    fn test_parse_input() {
        assert_eq!(
            parse_input(&vec![
                String::from("    [D]    "),
                String::from("[N] [C]    "),
                String::from("[Z] [M] [P]"),
                String::from(" 1   2   3"),
                String::from(""),
                String::from("move 1 from 2 to 1"),
                String::from("move 3 from 1 to 3"),
                String::from("move 2 from 2 to 1"),
                String::from("move 1 from 1 to 2"),
            ]),
            test_input()
        );
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(&test_input()), String::from("CMZ"))
    }
}
