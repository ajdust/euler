from util import fasta
from dna import *

def gcPerFasta(stream):
    for fastaChunk in fasta(stream):
        nc = dna([fastaChunk.content])
        yield ((nc.G + nc.C) / nc.total(), fastaChunk)

def gc(stream):
    maxGC = max(gcPerFasta(stream), key=lambda x: x[0])
    return f'{maxGC[1].label}\n{maxGC[0]}'



