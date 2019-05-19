use std::io::{BufReader};
use super::fasta;
use super::dna;

fn gc_chunk(chunk: &fasta::FastaChunk) -> f64 {
    let nc = dna::dna_count(&chunk.content);
    ((nc.g + nc.c) as f64) / (nc.total() as f64)
}

pub fn gc(reader: BufReader<std::fs::File>) -> String {
    let (max_gc, max_gc_chunk) = fasta::FastaFile::new(reader)
        .map(|chunk| (gc_chunk(&chunk), chunk))
        .max_by(|(gc, _), (gc2, _)| gc.partial_cmp(gc2).unwrap())
        .unwrap();

    format!("{}\n{}", max_gc_chunk.label, max_gc * 100.0)
}
