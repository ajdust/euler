
def to_rna(stream):
    for line in stream:
        for c in line:
            if c == "T":
                yield "U"
            else:
                yield c

def rna(stream):
    return "".join(to_rna(stream))
