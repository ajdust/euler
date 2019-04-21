import sys

def main():
    if sys.stdin.isatty():
        print('Please pipe input in.')
        return

    A = C = T = G = 0
    for line in sys.stdin:
        for c in line:
            if c == 'A':
                A += 1
            elif c == 'C':
                C += 1
            elif c == 'G':
                G += 1
            elif c == 'T':
                T += 1

    print(str(A) + " " + str(C) + " " + str(G) + " " + str(T))

if __name__ == "__main__":
    main()