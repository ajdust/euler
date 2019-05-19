use std::io::{BufReader, BufRead};

pub fn rna(reader: BufReader<std::fs::File>) -> String {
    let mut ns = Vec::new();
    for line in reader.lines() {
        let line = line.expect("Could not read the next line");
        for c in line.chars() {
            if c == 'T' {
                ns.push('U');
            } else {
                ns.push(c);
            }
        }
    }

    ns.iter().collect()
}