use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;

fn main() {
    let path = Path::new("inputs/day01.txt");
    let file = File::open(path).expect("cannot open file");
    let reader = io::BufReader::new(file);

    let mut max_elf_calories = 0;
    let mut elf_calories = 0;
    for line in reader.lines() {
        let line = line.expect("cannot read line");
        if line.trim().is_empty() {
            // Next elf
            elf_calories = 0;
        } else {
            let item_calories: i32 = line.trim().parse().expect("not a number");
            elf_calories += item_calories;
            max_elf_calories = max_elf_calories.max(elf_calories);
        }
    }

    println!("{max_elf_calories}");
}
