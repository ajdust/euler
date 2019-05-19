#[macro_use]
extern crate itertools;

use std::env;
use std::fs::File;
use std::io::{BufReader};

mod dna;
mod fib;
mod fasta;
mod cons;
mod gc;
mod hamm;
mod iprb;
mod prot;
mod revc;
mod rna;
mod subs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let default = String::from("subs");
    let problem = if args.len() >= 3 { &args[2] } else { &default };
    let file_path = if args.len() >= 4 { Some(&args[3]) } else { None };

    if problem == "cons" {
       let f = File::open(file_path.unwrap_or(&"../rosalind_cons.txt".to_string())).unwrap();
       println!("{}", cons::cons(BufReader::new(f)));
    } else if problem == "dna" {
        let f = File::open(file_path.unwrap_or(&"../rosalind_dna.txt".to_string())).unwrap();
        println!("{}", dna::dna(BufReader::new(f)));
    } else if problem == "fib" {
        println!("{}", fib::fib(33, 4));
    } else if problem == "gc" {
        let f = File::open(file_path.unwrap_or(&"../rosalind_gc.txt".to_string())).unwrap();
        println!("{}", gc::gc(BufReader::new(f)));
    } else if problem == "hamm" {
        let f = File::open(file_path.unwrap_or(&"../rosalind_hamm.txt".to_string())).unwrap();
        println!("{}", hamm::hamm(BufReader::new(f)));
    } else if problem == "iprb" {
        println!("{}", iprb::iprb(20, 18, 24));
    } else if problem == "prot" {
        let f = File::open(file_path.unwrap_or(&"../rosalind_prot.txt".to_string())).unwrap();
        println!("{}", prot::prot(BufReader::new(f)));
    } else if problem == "revc" {
        let f = File::open(file_path.unwrap_or(&"../rosalind_revc.txt".to_string())).unwrap();
        println!("{}", revc::revc(BufReader::new(f)));
    } else if problem == "rna" {
        let f = File::open(file_path.unwrap_or(&"../rosalind_rna.txt".to_string())).unwrap();
        println!("{}", rna::rna(BufReader::new(f)));
    } else if problem == "subs" {
        let f = File::open(file_path.unwrap_or(&"../rosalind_subs.txt".to_string())).unwrap();
        println!("{}", subs::subs(BufReader::new(f)));
    }
}
