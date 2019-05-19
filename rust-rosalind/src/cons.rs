use std::io::{BufReader};
use std::fmt;
use super::fasta;

pub struct VecInt(Vec<u32>);
pub struct Consensus {
    count_a: VecInt,
    count_c: VecInt,
    count_t: VecInt,
    count_g: VecInt,
    consensus: String
}

impl fmt::Display for Consensus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\nA: {}\nC: {}\nG: {}\nT: {}",
            self.consensus, self.count_a, self.count_c, self.count_g, self.count_t)
    }
}

impl fmt::Display for VecInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut total = String::new();
        for v in &self.0 {
            total.push_str(&v.to_string());
            total.push_str(" ");
        }
        write!(f, "{}", total)
    }
}

pub fn cons(reader: BufReader<std::fs::File>) -> Consensus {
    let mut count_a = Vec::new();
    let mut count_c = Vec::new();
    let mut count_g = Vec::new();
    let mut count_t = Vec::new();

    for chunk in fasta::FastaFile::new(reader) {
        if count_a.len() == 0 {
            let size = chunk.content.len();
            count_a = vec![0; size];
            count_c = vec![0; size];
            count_g = vec![0; size];
            count_t = vec![0; size];
        }

        for (i, c) in chunk.content.chars().enumerate() {
            if c == 'A' {
                count_a[i] += 1;
            } else if c == 'C' {
                count_c[i] += 1;
            } else if c == 'G' {
                count_g[i] += 1;
            } else if c == 'T' {
                count_t[i] += 1;
            }
        }
    }

    let mut consensus = Vec::new();
    for (a, c, g, t) in izip!(&count_a, &count_c, &count_g, &count_t) {
        let mut topn = "A";
        let mut topcount = a;
        if c > topcount {
            topn = "C";
            topcount = c;
        }
        if g > topcount {
            topn = "G";
            topcount = g;
        }
        if t > topcount {
            topn = "T";
        }

        consensus.push(topn);
    }

    return Consensus {
        consensus: consensus.join(""),
        count_a: VecInt(count_a),
        count_c: VecInt(count_c),
        count_t: VecInt(count_t),
        count_g: VecInt(count_g)
    };
}
