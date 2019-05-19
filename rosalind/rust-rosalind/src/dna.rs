use std::io::{BufRead, BufReader};
use std::fmt;

pub struct NucleotideCount {
    pub a: u32,
    pub c: u32,
    pub g: u32,
    pub t: u32
}

impl NucleotideCount {
    pub fn total(&self) -> u32 {
        self.a + self.c + self.g + self.t
    }
}

impl fmt::Display for NucleotideCount {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {} {}", self.a, self.c, self.g, self.t)
    }
}

pub fn dna_count(s: &String) -> NucleotideCount {
    let (mut a, mut c, mut g, mut t) = (0, 0, 0, 0);
    for letter in s.chars() {
        if letter == 'A' { a += 1; }
        else if letter == 'C' { c += 1; }
        else if letter == 'G' { g += 1; }
        else if letter == 'T' { t += 1; }
    }

    NucleotideCount { a: a, c: c, g: g, t: t }
}

pub fn dna(reader: BufReader<std::fs::File>) -> NucleotideCount {
    let (mut a, mut c, mut g, mut t) = (0, 0, 0, 0);
    for line in reader.lines() {
        let line = line.expect("Could not read next line");
        for letter in line.chars() {
            if letter == 'A' { a += 1; }
            else if letter == 'C' { c += 1; }
            else if letter == 'G' { g += 1; }
            else if letter == 'T' { t += 1; }
        }
    }

    NucleotideCount { a: a, c: c, g: g, t: t }
}