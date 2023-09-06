use std::fs::File;
use std::io;
use std::io::BufRead;
use std::path::Path;

pub fn slurp_input(day: u8) -> Vec<String> {
    let mut input = Vec::new();
    let filename = format!("inputs/day{:02}.txt", day);
    let path = Path::new(&filename);
    let file = File::open(path).expect("cannot open file");
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        input.push(line.expect("cannot read line"));
    }
    input
}
