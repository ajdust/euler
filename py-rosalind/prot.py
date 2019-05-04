from enum import Enum, auto
from typing import Generator

class AminoAcid(Enum):
    A = auto()
    C = auto()
    D = auto()
    E = auto()
    F = auto()
    G = auto()
    H = auto()
    I = auto()
    K = auto()
    L = auto()
    M = auto()
    N = auto()
    P = auto()
    Q = auto()
    R = auto()
    S = auto()
    T = auto()
    V = auto()
    W = auto()
    Y = auto()
    Stop = auto()

def convertToAminoAcid(codon: str) -> AminoAcid:
    if codon == "UUU":
        return AminoAcid.F
    elif codon == "CUU":
        return AminoAcid.L
    elif codon == "AUU":
        return AminoAcid.I
    elif codon == "GUU":
        return AminoAcid.V
    elif codon == "UUC":
        return AminoAcid.F
    elif codon == "CUC":
        return AminoAcid.L
    elif codon == "AUC":
        return AminoAcid.I
    elif codon == "GUC":
        return AminoAcid.V
    elif codon == "UUA":
        return AminoAcid.L
    elif codon == "CUA":
        return AminoAcid.L
    elif codon == "AUA":
        return AminoAcid.I
    elif codon == "GUA":
        return AminoAcid.V
    elif codon == "UUG":
        return AminoAcid.L
    elif codon == "CUG":
        return AminoAcid.L
    elif codon == "AUG":
        return AminoAcid.M
    elif codon == "GUG":
        return AminoAcid.V
    elif codon == "UCU":
        return AminoAcid.S
    elif codon == "CCU":
        return AminoAcid.P
    elif codon == "ACU":
        return AminoAcid.T
    elif codon == "GCU":
        return AminoAcid.A
    elif codon == "UCC":
        return AminoAcid.S
    elif codon == "CCC":
        return AminoAcid.P
    elif codon == "ACC":
        return AminoAcid.T
    elif codon == "GCC":
        return AminoAcid.A
    elif codon == "UCA":
        return AminoAcid.S
    elif codon == "CCA":
        return AminoAcid.P
    elif codon == "ACA":
        return AminoAcid.T
    elif codon == "GCA":
        return AminoAcid.A
    elif codon == "UCG":
        return AminoAcid.S
    elif codon == "CCG":
        return AminoAcid.P
    elif codon == "ACG":
        return AminoAcid.T
    elif codon == "GCG":
        return AminoAcid.A
    elif codon == "UAU":
        return AminoAcid.Y
    elif codon == "CAU":
        return AminoAcid.H
    elif codon == "AAU":
        return AminoAcid.N
    elif codon == "GAU":
        return AminoAcid.D
    elif codon == "UAC":
        return AminoAcid.Y
    elif codon == "CAC":
        return AminoAcid.H
    elif codon == "AAC":
        return AminoAcid.N
    elif codon == "GAC":
        return AminoAcid.D
    elif codon == "UAA":
        return AminoAcid.Stop
    elif codon == "CAA":
        return AminoAcid.Q
    elif codon == "AAA":
        return AminoAcid.K
    elif codon == "GAA":
        return AminoAcid.E
    elif codon == "UAG":
        return AminoAcid.Stop
    elif codon == "CAG":
        return AminoAcid.Q
    elif codon == "AAG":
        return AminoAcid.K
    elif codon == "GAG":
        return AminoAcid.E
    elif codon == "UGU":
        return AminoAcid.C
    elif codon == "CGU":
        return AminoAcid.R
    elif codon == "AGU":
        return AminoAcid.S
    elif codon == "GGU":
        return AminoAcid.G
    elif codon == "UGC":
        return AminoAcid.C
    elif codon == "CGC":
        return AminoAcid.R
    elif codon == "AGC":
        return AminoAcid.S
    elif codon == "GGC":
        return AminoAcid.G
    elif codon == "UGA":
        return AminoAcid.Stop
    elif codon == "CGA":
        return AminoAcid.R
    elif codon == "AGA":
        return AminoAcid.R
    elif codon == "GGA":
        return AminoAcid.G
    elif codon == "UGG":
        return AminoAcid.W
    elif codon == "CGG":
        return AminoAcid.R
    elif codon == "AGG":
        return AminoAcid.R
    elif codon == "GGG":
        return AminoAcid.G
    else:
        raise Exception(f"Invalid codon {codon}")

def convertToString(aa: AminoAcid) -> str:
    if aa == AminoAcid.A:
        return "A"
    elif aa == AminoAcid.C:
        return "C"
    elif aa == AminoAcid.D:
        return "D"
    elif aa == AminoAcid.E:
        return "E"
    elif aa == AminoAcid.F:
        return "F"
    elif aa == AminoAcid.G:
        return "G"
    elif aa == AminoAcid.H:
        return "H"
    elif aa == AminoAcid.I:
        return "I"
    elif aa == AminoAcid.K:
        return "K"
    elif aa == AminoAcid.L:
        return "L"
    elif aa == AminoAcid.M:
        return "M"
    elif aa == AminoAcid.N:
        return "N"
    elif aa == AminoAcid.P:
        return "P"
    elif aa == AminoAcid.Q:
        return "Q"
    elif aa == AminoAcid.R:
        return "R"
    elif aa == AminoAcid.S:
        return "S"
    elif aa == AminoAcid.T:
        return "T"
    elif aa == AminoAcid.V:
        return "V"
    elif aa == AminoAcid.W:
        return "W"
    elif aa == AminoAcid.Y:
        return "Y"
    elif aa == AminoAcid.Stop:
        return "M"
    else:
        raise Exception(f"Invalid AminoAcid {aa}")

def codonGenerator(stream) -> Generator[str, None, None]:
    for line in stream:
        i = line.find("AUG")
        for ci in range(i, len(line) - 2, 3):
            yield convertToAminoAcid(line[ci: ci + 3])

def prot(stream):
    return "".join([convertToString(aa) for aa in codonGenerator(stream)])






