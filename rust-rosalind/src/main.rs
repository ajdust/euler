use std::env;
use std::io::{self, BufRead};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        println!("Please pipe input in.");
        return
    }

    let (mut a, mut c, mut g, mut t) = (0, 0, 0, 0);
    for line in io::stdin().lock().lines() {
        let line = line.expect("Could not read line from standard in");
        for letter in line.chars() {
            if letter == 'A' { a += 1; }
            else if letter == 'C' { c += 1; }
            else if letter == 'G' { g += 1; }
            else if letter == 'T' { t += 1; }
        }
    }

    println!("{} {} {} {}", a, c, g, t);
}
