use std::io::{BufReader, BufRead};

fn hamm_count(left: String, right: String) -> u32 {
    left.chars().zip(right.chars()).map(|(l, r)| if l != r { 1 } else { 0 }).sum()
}

pub fn hamm(reader: BufReader<std::fs::File>) -> u32 {
    let mut line_it = reader.lines();
    let line1 = line_it.next().unwrap().unwrap();
    let line2 = line_it.next().unwrap().unwrap();
    hamm_count(line1, line2)
}