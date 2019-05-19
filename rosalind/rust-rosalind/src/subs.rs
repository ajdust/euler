use std::io::{BufReader, BufRead};

fn find_substr_indexes(big: String, small: String) -> Vec<usize> {
    let small_ = &small[..];
    let mut pos = 0;
    let mut indexes = Vec::new();
    while let Some(i) = (&big[pos..]).find(small_) {
        indexes.push(i + pos);
        pos += i + 1;
    }

    indexes
}

pub fn subs(reader: BufReader<std::fs::File>) -> String {
    let mut lines_it = reader.lines();
    let line1 = lines_it.next().expect("Could not read the next line").unwrap();
    let line2 = lines_it.next().expect("Could not read the next line").unwrap();
    find_substr_indexes(line1, line2)
        .iter()
        .map(|i| (i + 1).to_string())
        .collect::<Vec<String>>()
        .join(" ")
}
