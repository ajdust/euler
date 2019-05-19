from typing import List

def findSubstrs(big: str, small: str) -> List[int]:
    pos = 0
    i = big.find(small)
    indexes: List[int] = []
    while i != -1:
        indexes.append(i)
        pos = i + 1
        i = big.find(small, pos)

    return indexes

def subs(stream) -> str:
    line1 = next(stream)
    line2 = next(stream)
    return " ".join([str(x + 1) for x in findSubstrs(line1.strip(), line2.strip())])
