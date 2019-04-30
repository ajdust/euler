from typing import Generator

class FastaChunk:
    def __init__(self, label: str, content: str):
        self.label = label.strip()
        self.content = content.strip()

def fasta(stream) -> Generator[FastaChunk, None, None]:
    label = ""
    content = ""

    for line in stream:
        if not line or len(line) < 2:
            continue
        if line[0] == ">":
            if label:
                yield FastaChunk(label, content)
                label = ""
                content = ""

            label = line.lstrip(">")
        else:
            content += line

    if label:
        yield FastaChunk(label, content)
