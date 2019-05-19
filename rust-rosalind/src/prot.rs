use std::io::{BufReader, BufRead};

#[derive(PartialEq)]
pub enum AminoAcid {
    A,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    K,
    L,
    M,
    N,
    P,
    Q,
    R,
    S,
    T,
    V,
    W,
    Y,
    Stop,
}

pub fn convert_codon_to_aa(codon: &str) -> Option<AminoAcid> {
    if codon == "UUU" {
        Some(AminoAcid::F)
    } else if codon == "CUU" {
        Some(AminoAcid::L)
    } else if codon == "AUU" {
        Some(AminoAcid::I)
    } else if codon == "GUU" {
        Some(AminoAcid::V)
    } else if codon == "UUC" {
        Some(AminoAcid::F)
    } else if codon == "CUC" {
        Some(AminoAcid::L)
    } else if codon == "AUC" {
        Some(AminoAcid::I)
    } else if codon == "GUC" {
        Some(AminoAcid::V)
    } else if codon == "UUA" {
        Some(AminoAcid::L)
    } else if codon == "CUA" {
        Some(AminoAcid::L)
    } else if codon == "AUA" {
        Some(AminoAcid::I)
    } else if codon == "GUA" {
        Some(AminoAcid::V)
    } else if codon == "UUG" {
        Some(AminoAcid::L)
    } else if codon == "CUG" {
        Some(AminoAcid::L)
    } else if codon == "AUG" {
        Some(AminoAcid::M)
    } else if codon == "GUG" {
        Some(AminoAcid::V)
    } else if codon == "UCU" {
        Some(AminoAcid::S)
    } else if codon == "CCU" {
        Some(AminoAcid::P)
    } else if codon == "ACU" {
        Some(AminoAcid::T)
    } else if codon == "GCU" {
        Some(AminoAcid::A)
    } else if codon == "UCC" {
        Some(AminoAcid::S)
    } else if codon == "CCC" {
        Some(AminoAcid::P)
    } else if codon == "ACC" {
        Some(AminoAcid::T)
    } else if codon == "GCC" {
        Some(AminoAcid::A)
    } else if codon == "UCA" {
        Some(AminoAcid::S)
    } else if codon == "CCA" {
        Some(AminoAcid::P)
    } else if codon == "ACA" {
        Some(AminoAcid::T)
    } else if codon == "GCA" {
        Some(AminoAcid::A)
    } else if codon == "UCG" {
        Some(AminoAcid::S)
    } else if codon == "CCG" {
        Some(AminoAcid::P)
    } else if codon == "ACG" {
        Some(AminoAcid::T)
    } else if codon == "GCG" {
        Some(AminoAcid::A)
    } else if codon == "UAU" {
        Some(AminoAcid::Y)
    } else if codon == "CAU" {
        Some(AminoAcid::H)
    } else if codon == "AAU" {
        Some(AminoAcid::N)
    } else if codon == "GAU" {
        Some(AminoAcid::D)
    } else if codon == "UAC" {
        Some(AminoAcid::Y)
    } else if codon == "CAC" {
        Some(AminoAcid::H)
    } else if codon == "AAC" {
        Some(AminoAcid::N)
    } else if codon == "GAC" {
        Some(AminoAcid::D)
    } else if codon == "UAA" {
        Some(AminoAcid::Stop)
    } else if codon == "CAA" {
        Some(AminoAcid::Q)
    } else if codon == "AAA" {
        Some(AminoAcid::K)
    } else if codon == "GAA" {
        Some(AminoAcid::E)
    } else if codon == "UAG" {
        Some(AminoAcid::Stop)
    } else if codon == "CAG" {
        Some(AminoAcid::Q)
    } else if codon == "AAG" {
        Some(AminoAcid::K)
    } else if codon == "GAG" {
        Some(AminoAcid::E)
    } else if codon == "UGU" {
        Some(AminoAcid::C)
    } else if codon == "CGU" {
        Some(AminoAcid::R)
    } else if codon == "AGU" {
        Some(AminoAcid::S)
    } else if codon == "GGU" {
        Some(AminoAcid::G)
    } else if codon == "UGC" {
        Some(AminoAcid::C)
    } else if codon == "CGC" {
        Some(AminoAcid::R)
    } else if codon == "AGC" {
        Some(AminoAcid::S)
    } else if codon == "GGC" {
        Some(AminoAcid::G)
    } else if codon == "UGA" {
        Some(AminoAcid::Stop)
    } else if codon == "CGA" {
        Some(AminoAcid::R)
    } else if codon == "AGA" {
        Some(AminoAcid::R)
    } else if codon == "GGA" {
        Some(AminoAcid::G)
    } else if codon == "UGG" {
        Some(AminoAcid::W)
    } else if codon == "CGG" {
        Some(AminoAcid::R)
    } else if codon == "AGG" {
        Some(AminoAcid::R)
    } else if codon == "GGG" {
        Some(AminoAcid::G)
    } else {
        None
    }
}

pub fn aa_to_str(aa: &AminoAcid) -> Option<String> {
    match aa {
        AminoAcid::A => Some(String::from("A")),
        AminoAcid::C => Some(String::from("C")),
        AminoAcid::D => Some(String::from("D")),
        AminoAcid::E => Some(String::from("E")),
        AminoAcid::F => Some(String::from("F")),
        AminoAcid::G => Some(String::from("G")),
        AminoAcid::H => Some(String::from("H")),
        AminoAcid::I => Some(String::from("I")),
        AminoAcid::K => Some(String::from("K")),
        AminoAcid::L => Some(String::from("L")),
        AminoAcid::M => Some(String::from("M")),
        AminoAcid::N => Some(String::from("N")),
        AminoAcid::P => Some(String::from("P")),
        AminoAcid::Q => Some(String::from("Q")),
        AminoAcid::R => Some(String::from("R")),
        AminoAcid::S => Some(String::from("S")),
        AminoAcid::T => Some(String::from("T")),
        AminoAcid::V => Some(String::from("V")),
        AminoAcid::W => Some(String::from("W")),
        AminoAcid::Y => Some(String::from("Y")),
        AminoAcid::Stop => Some(String::from("M")),
    }
}

pub fn prot(reader: BufReader<std::fs::File>) -> String {
    let mut aas: Vec<AminoAcid> = Vec::new();
    for line in reader.lines() {
        let line = line.expect("Could not read the next line");
        let i = line.find("AUG").unwrap();
        for ci in (i..line.len() - 2).step_by(3) {
            if let Some(aa) = convert_codon_to_aa(&line[ci..ci + 3]) {
                aas.push(aa);
            }
        }
    }

    aas.iter()
        .map(|aa| aa_to_str(&aa).unwrap())
        .collect::<Vec<String>>().join("")
}