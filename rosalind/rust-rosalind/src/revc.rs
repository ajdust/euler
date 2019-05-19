use std::io::{BufReader, BufRead};

pub fn complement(c: char) -> Option<char> {
    match c {
        'A' => Some('T'),
        'C' => Some('G'),
        'G' => Some('C'),
        'T' => Some('A'),
        _ => None
    }
}

pub fn revc(reader: BufReader<std::fs::File>) -> String {
    let mut ns = Vec::new();
    for line in reader.lines() {
        let line = line.expect("Could not read the next line");
        for c in line.chars() {
            if let Some(complement) = complement(c) {
                ns.push(complement)
            }
        }
    }

    ns.iter().rev().collect()
}