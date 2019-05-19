use std::io::{Lines, BufRead, BufReader};

pub struct FastaChunk {
    pub label: String,
    pub content: String
}

pub struct FastaFile {
    current_chunk_number: u32,
    lines: Lines<BufReader<std::fs::File>>,
    next_label: Option<String>
}

impl Iterator for FastaFile {
    type Item = FastaChunk;
    fn next(&mut self) -> Option<Self::Item> {

        let mut label = self.next_label.clone();
        let mut content = Vec::<String>::new();

        while let Some(Ok(line)) = self.lines.next() {
            // we have detected the start of a chunk!
            if line.starts_with(">") {
                self.current_chunk_number += 1;
                if self.current_chunk_number == 1 {
                    // start building the first chunk
                    label = Some(line.chars().skip(1).collect());
                } else if let Some(label) = label {
                    // we have a chunk, put it out and start the next one
                    self.next_label = Some(line.chars().skip(1).collect());
                    return Some(FastaChunk { label: label, content: content.join("") });
                }
            } else {
                content.push(line.clone());
            }
        }

        if let Some(label) = label {
            self.next_label = None;
            return Some(FastaChunk { label: label, content: content.join("") });
        }

        None
    }
}

impl FastaFile {
    pub fn new(reader: BufReader<std::fs::File>) -> FastaFile {
        let m = reader.lines();
        FastaFile { lines: m, current_chunk_number: 0, next_label: None }
    }
}