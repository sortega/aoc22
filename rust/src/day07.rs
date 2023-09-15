use crate::common;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, newline, one_of, u32};
use nom::combinator::map_opt;
use nom::sequence::{preceded, separated_pair};
use nom::{IResult};
use std::collections::HashMap;
use nom::multi::{many1, separated_list0};
use Command::ListDir;
use Command::ChangeDir;

fn parse_filename(input: &str) -> IResult<&str, String> {
    map_opt(
        many1(one_of("abcdefghijklmnopqrstuvwxyz.")),
    |chars: Vec<char>| Some(chars.iter().collect() ))(input)
}

#[derive(PartialEq, Debug)]
enum Target {
    Root,
    Up,
    Down(String),
}

impl Target {
    fn parse(input: &str) -> IResult<&str, Target> {
        alt((
            map_opt(tag("/"), |_| Some(Target::Root)),
            map_opt(tag(".."), |_| Some(Target::Up)),
            map_opt(alphanumeric1, |name: &str| {
                Some(Target::Down(name.to_string()))
            }),
        ))(input)
    }
}

#[derive(PartialEq, Debug)]
enum ListEntry {
    Directory { name: String },
    File { name: String, size: usize },
}

impl ListEntry {
    fn parse(input: &str) -> IResult<&str, ListEntry> {
        alt((
            map_opt(preceded(tag("dir "), parse_filename), |name| {
                Some(ListEntry::Directory {
                    name,
                })
            }),
            map_opt(
                separated_pair(u32, tag(" "), parse_filename),
                |(size, name): (u32, String)|
                    Some(ListEntry::File {
                        name: name,
                        size: size as usize,
                    })
                ,
            ),
        ))(input)
    }
}

#[derive(PartialEq, Debug)]
enum Command {
    ChangeDir(Target),
    ListDir(Vec<ListEntry>),
}

impl Command {
    fn parse(input: &str) -> IResult<&str, Command> {
        alt((
            map_opt(preceded(tag("$ cd "), Target::parse), |target| {
                Some(ChangeDir(target))
            }),
            map_opt(preceded(tag("$ ls\n"), separated_list0(newline,ListEntry::parse)), |listing| {
                Some(ListDir(listing))
            }),
        ))(input)
    }
}

type Input = Vec<Command>;

fn parse_input(input: &str) -> Input {
    let (remainder, commands ) = separated_list0(newline, Command::parse)(input).expect("cannot parse");
    assert!(remainder.trim().is_empty());
    commands
}

type Path = Vec<String>;

fn directory_sizes(input: &Input) -> HashMap<Vec<String>, usize> {
    let mut sizes = HashMap::new();
    let mut path: Path = Vec::new();
    sizes.insert(path.clone(), 0);
    for command in input {
        match command {
            Command::ChangeDir(Target::Root) => path.clear(),
            Command::ChangeDir(Target::Up) => {
                path.pop();
            }
            Command::ChangeDir(Target::Down(sub_dir)) => path.push(sub_dir.clone()),
            ListDir(entries) => {
                for entry in entries {
                    match entry {
                        ListEntry::Directory { name } => {
                            let mut sub_dir = path.clone();
                            sub_dir.push(name.clone());
                            sizes.insert(sub_dir, 0);
                        }
                        ListEntry::File { name: _, size } => {
                            for i in 0..(path.len() + 1) {
                                let dir_size = sizes.get_mut(&path[..i]).expect("doesn't exist");
                                *dir_size += size;
                            }
                        }
                    }
                }
            }
        }
    }
    sizes
}

fn part1(input: &Input) -> usize {
    let sizes = directory_sizes(input);
    sizes.values().filter(|size| **size <= 100000).sum()
}

const DISK_SIZE: usize = 70000000;
const TARGET_FREE_SPACE: usize = 30000000;

fn part2(input: &Input) -> usize {
    let sizes = directory_sizes(input);
    let free_space = DISK_SIZE - sizes.get(&Vec::new()).unwrap();
    let to_be_freed = TARGET_FREE_SPACE - free_space;
    *sizes
        .values()
        .filter(|size| **size >= to_be_freed)
        .min()
        .unwrap()
}

pub fn solve() {
    let lines = common::slurp(7);
    let input = parse_input(&lines);
    println!(
        "DAY 07\n======\npart 1: {}\npart 2: {}\n",
        part1(&input),
        part2(&input)
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use ListEntry::*;

    fn test_input() -> Input {
        vec![
            ChangeDir(Target::Root),
            ListDir(vec![
                Directory {
                    name: String::from("a"),
                },
                File {
                    name: String::from("b.txt"),
                    size: 14848514,
                },
                File {
                    name: String::from("c.dat"),
                    size: 8504156,
                },
                Directory {
                    name: String::from("d"),
                },
            ]),
            ChangeDir(Target::Down(String::from("a"))),
            ListDir(vec![
                Directory {
                    name: String::from("e"),
                },
                File {
                    name: String::from("f"),
                    size: 29116,
                },
                File {
                    name: String::from("g"),
                    size: 2557,
                },
                File {
                    name: String::from("h.lst"),
                    size: 62596,
                },
            ]),
            ChangeDir(Target::Down(String::from("e"))),
            ListDir(vec![File {
                name: String::from("i"),
                size: 584,
            }]),
            ChangeDir(Target::Up),
            ChangeDir(Target::Up),
            ChangeDir(Target::Down(String::from("d"))),
            ListDir(vec![
                File {
                    name: String::from("j"),
                    size: 4060174,
                },
                File {
                    name: String::from("d.log"),
                    size: 8033020,
                },
                File {
                    name: String::from("d.ext"),
                    size: 5626152,
                },
                File {
                    name: String::from("k"),
                    size: 7214296,
                },
            ]),
        ]
    }

    #[test]
    fn test_parse() {
        let input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
";
        assert_eq!(parse_input(&input), test_input());
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(&test_input()), 95437);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&test_input()), 24933642);
    }
}
