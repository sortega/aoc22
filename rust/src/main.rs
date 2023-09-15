#[macro_use]
extern crate scan_fmt;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;

mod common;

fn main() {
    day01::solve();
    day02::solve();
    day03::solve();
    day04::solve();
    day05::solve();
    day06::solve();
    day07::solve();
}
