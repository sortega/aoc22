use crate::common::slurp_lines;

struct Inventory {
    item_calories: Vec<i32>,
}

impl Inventory {
    fn calories(&self) -> i32 {
        self.item_calories.iter().sum()
    }
}

type Input = Vec<Inventory>;

pub fn solve() {
    let maybe_calories: Vec<Option<i32>> = slurp_lines(1)
        .iter()
        .map(|line| line.trim().parse().ok())
        .into_iter()
        .collect();
    let mut input: Input = maybe_calories
        .split(|option| option.is_none())
        .map(|items| Inventory {
            item_calories: items.iter().filter_map(|r| *r).collect(),
        })
        .collect();

    input.sort_by_cached_key(|inventory| -inventory.calories());
    input.truncate(3);

    let part1 = input.first().unwrap().calories();
    let part2: i32 = input.iter().map(|inventory| inventory.calories()).sum();
    println!("DAY 01\n======\npart 1: {part1}\npart 2: {part2}\n");
}
