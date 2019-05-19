class NucleotideCount:
    def __init__(self, a, c, g, t):
        self.A = a
        self.C = c
        self.G = g
        self.T = t

    def __str__(self):
        return f'{self.A} {self.C} {self.G} {self.T}'

    def total(self):
        return self.A + self.C + self.G + self.T

def dna(stream):
    A = C = T = G = 0
    for line in stream:
        for c in line:
            if c == 'A':
                A += 1
            elif c == 'C':
                C += 1
            elif c == 'G':
                G += 1
            elif c == 'T':
                T += 1

    return NucleotideCount(A, C, G, T)
