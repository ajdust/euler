from typing import List
from util import fasta

def cons(stream):

    countA: List[int] = []
    countC: List[int] = []
    countG: List[int] = []
    countT: List[int] = []

    for chunk in fasta(stream):

        if len(countA) == 0:
            dnaSize = len(chunk.content)
            countA = [0 for _ in range(dnaSize)]
            countC = [0 for _ in range(dnaSize)]
            countG = [0 for _ in range(dnaSize)]
            countT = [0 for _ in range(dnaSize)]

        for i, c in enumerate(chunk.content):
            if c == "A":
                countA[i] += 1
            elif c == "C":
                countC[i] += 1
            elif c == "G":
                countG[i] += 1
            elif c == "T":
                countT[i] += 1

    consensus = ""
    for a, c, g, t in zip(countA, countC, countG, countT):
        consensus += max([
            {"l": "A", "n": a},
            {"l": "C", "n": c},
            {"l": "G", "n": g},
            {"l": "T", "n": t},
        ], key=lambda x: x["n"])["l"]

    return { "A": countA, "C": countC, "G": countG, "T": countT, "Consensus": consensus }
