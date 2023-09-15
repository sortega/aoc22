use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;

fn input_filename(day: u8) -> String {
    format!("inputs/day{:02}.txt", day)
}

pub fn slurp(day: u8) -> String {
    std::fs::read_to_string(input_filename(day)).expect("cannot open file")
}

pub fn slurp_lines(day: u8) -> Vec<String> {
    let mut input = Vec::new();
    let filename = input_filename(day);
    let path = Path::new(&filename);
    let file = File::open(path).expect("cannot open file");
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        input.push(line.expect("cannot read line"));
    }
    input
}
