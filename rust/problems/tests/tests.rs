extern crate serde;
extern crate serde_json;
extern crate problems;

use serde_json::Map;
use std::error::Error;
use std::fs::{File,canonicalize};
use std::io::prelude::*;
use std::path::Path;
use problems::*;

fn answer_for(n: u32) -> i64 {

    fn io_fail(action: &str, path: &Path, why: &str) -> String {
        let filename = path.file_name().expect("No file name");
        let parent = path.parent().expect("No parent dir");
        let abs_parent = canonicalize(&(Path::new(parent))).expect("No absolute path");
        format!("Could not {} {:?} in {:?}: {}", action, filename, abs_parent, why)
    }

    let ans_path = Path::new("../../answers.json");
    let mut file = match File::open(&ans_path) {
        Ok(file) => file,
        Err(why) => panic!(io_fail("open", &ans_path, why.description())),
    };

    let mut s = String::new();
    if let Err(why) = file.read_to_string(&mut s) {
        panic!(io_fail("read", &ans_path, why.description()));
    }

    let answers: Map<u32, i64> = serde_json::from_str(&s).unwrap();
    match answers.get(&n) {
        Some(answer) => *answer,
        None => panic!("Answer for {} not found", n)
    }
}

#[test]
fn problem01_is_correct() {
    let solution = problem01::solve() as i64;
    assert_eq!(answer_for(1), solution);
}

#[test]
fn problem02_is_correct() {
    let solution = problem02::solve() as i64;
    assert_eq!(answer_for(2), solution);
}

#[test]
fn problem03_is_correct() {
    let solution = problem03::solve() as i64;
    assert_eq!(answer_for(3), solution);
}