fn main() {
    let input = std::fs::read_to_string("input/day01.txt").unwrap();
    let mut elves: Vec<u32> = input
        .split("\n\n")
        .map(|c| c.lines().map(|c| c.parse::<u32>().unwrap()).sum())
        .collect();

    elves.sort();

    println!("Part 1: {:?}", elves.last().unwrap());
    println!("Part 1: {:?}", elves.iter().rev().take(3).sum::<u32>());
}
