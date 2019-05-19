
def complement(stream):
    for line in stream:
        for c in line:
            if c == "A":
                yield "T"
            elif c == "C":
                yield "G"
            elif c == "G":
                yield "C"
            elif c == "T":
                yield "A"

def revc(stream):
    return "".join(complement(stream))[::-1]
