use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;

fn main() {
    let path = Path::new("inputs/day01.txt");
    let file = File::open(path).expect("cannot open file");
    let reader = io::BufReader::new(file);

    let mut elves: Vec<i32> = Vec::new();
    let mut current_elf = 0;
    for line in reader.lines() {
        let line = line.expect("cannot read line");
        if line.trim().is_empty() {
            elves.push(current_elf);
            current_elf = 0;
        } else {
            let item_calories: i32 = line.trim().parse().expect("not a number");
            current_elf += item_calories;
        }
    }

    elves.sort();
    elves.reverse();
    elves.truncate(3);

    let part1 = elves.first().unwrap();
    let part2: i32 = elves.iter().sum();
    println!("part 1: {part1}\npart 2: {part2}");
}
