import sys
from dna import dna
from cons import cons
from fib import fib
from rosalindgc import gc
from typing import List, TextIO
from hamm import hamm
from iprb import iprb
from prot import prot
from revc import revc
from rna import rna
from subs import subs

def main(args: List[str]):
    problem = args[2] if len(args) >= 3 else "subs"
    filePath = args[3] if len(args) >= 4 else None

    if problem == "dna":
        with open(filePath if filePath is not None else "./rosalind_dna.txt") as f:
            print(dna(f))
    elif problem == "cons":
        with open(filePath if filePath is not None else "./rosalind_cons.txt") as f:
            print(cons(f))
    elif problem == "fib":
        print(fib(33, 4))
    elif problem == "gc":
        with open(filePath if filePath is not None else "./rosalind_gc.txt") as f:
            print(gc(f))
    elif problem == "hamm":
        with open(filePath if filePath is not None else "./rosalind_hamm.txt") as f:
            print(hamm(f))
    elif problem == "iprb":
        print(iprb(20, 18, 24))
    elif problem == "prot":
        with open(filePath if filePath is not None else "../rosalind_prot.txt") as f:
            print(prot(f))
    elif problem == "revc":
        with open(filePath if filePath is not None else "../rosalind_revc.txt") as f:
            print(revc(f))
    elif problem == "rna":
        with open(filePath if filePath is not None else "../rosalind_rna.txt") as f:
            print(rna(f))
    elif problem == "subs":
        with open(filePath if filePath is not None else "../rosalind_subs.txt") as f:
            print(subs(f))

if __name__ == "__main__":
    main(sys.argv)
