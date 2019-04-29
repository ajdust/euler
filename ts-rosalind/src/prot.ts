import stream = require("stream");

enum AminoAcid {
    A,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    K,
    L,
    M,
    N,
    P,
    Q,
    R,
    S,
    T,
    V,
    W,
    Y,
    Stop,
}

export function convertToAminoAcid(codon: string): AminoAcid {
    if (codon === "UUU") {
        return AminoAcid.F;
    } else if (codon === "CUU") {
        return AminoAcid.L;
    } else if (codon === "AUU") {
        return AminoAcid.I;
    } else if (codon === "GUU") {
        return AminoAcid.V;
    } else if (codon === "UUC") {
        return AminoAcid.F;
    } else if (codon === "CUC") {
        return AminoAcid.L;
    } else if (codon === "AUC") {
        return AminoAcid.I;
    } else if (codon === "GUC") {
        return AminoAcid.V;
    } else if (codon === "UUA") {
        return AminoAcid.L;
    } else if (codon === "CUA") {
        return AminoAcid.L;
    } else if (codon === "AUA") {
        return AminoAcid.I;
    } else if (codon === "GUA") {
        return AminoAcid.V;
    } else if (codon === "UUG") {
        return AminoAcid.L;
    } else if (codon === "CUG") {
        return AminoAcid.L;
    } else if (codon === "AUG") {
        return AminoAcid.M;
    } else if (codon === "GUG") {
        return AminoAcid.V;
    } else if (codon === "UCU") {
        return AminoAcid.S;
    } else if (codon === "CCU") {
        return AminoAcid.P;
    } else if (codon === "ACU") {
        return AminoAcid.T;
    } else if (codon === "GCU") {
        return AminoAcid.A;
    } else if (codon === "UCC") {
        return AminoAcid.S;
    } else if (codon === "CCC") {
        return AminoAcid.P;
    } else if (codon === "ACC") {
        return AminoAcid.T;
    } else if (codon === "GCC") {
        return AminoAcid.A;
    } else if (codon === "UCA") {
        return AminoAcid.S;
    } else if (codon === "CCA") {
        return AminoAcid.P;
    } else if (codon === "ACA") {
        return AminoAcid.T;
    } else if (codon === "GCA") {
        return AminoAcid.A;
    } else if (codon === "UCG") {
        return AminoAcid.S;
    } else if (codon === "CCG") {
        return AminoAcid.P;
    } else if (codon === "ACG") {
        return AminoAcid.T;
    } else if (codon === "GCG") {
        return AminoAcid.A;
    } else if (codon === "UAU") {
        return AminoAcid.Y;
    } else if (codon === "CAU") {
        return AminoAcid.H;
    } else if (codon === "AAU") {
        return AminoAcid.N;
    } else if (codon === "GAU") {
        return AminoAcid.D;
    } else if (codon === "UAC") {
        return AminoAcid.Y;
    } else if (codon === "CAC") {
        return AminoAcid.H;
    } else if (codon === "AAC") {
        return AminoAcid.N;
    } else if (codon === "GAC") {
        return AminoAcid.D;
    } else if (codon === "UAA") {
        return AminoAcid.Stop;
    } else if (codon === "CAA") {
        return AminoAcid.Q;
    } else if (codon === "AAA") {
        return AminoAcid.K;
    } else if (codon === "GAA") {
        return AminoAcid.E;
    } else if (codon === "UAG") {
        return AminoAcid.Stop;
    } else if (codon === "CAG") {
        return AminoAcid.Q;
    } else if (codon === "AAG") {
        return AminoAcid.K;
    } else if (codon === "GAG") {
        return AminoAcid.E;
    } else if (codon === "UGU") {
        return AminoAcid.C;
    } else if (codon === "CGU") {
        return AminoAcid.R;
    } else if (codon === "AGU") {
        return AminoAcid.S;
    } else if (codon === "GGU") {
        return AminoAcid.G;
    } else if (codon === "UGC") {
        return AminoAcid.C;
    } else if (codon === "CGC") {
        return AminoAcid.R;
    } else if (codon === "AGC") {
        return AminoAcid.S;
    } else if (codon === "GGC") {
        return AminoAcid.G;
    } else if (codon === "UGA") {
        return AminoAcid.Stop;
    } else if (codon === "CGA") {
        return AminoAcid.R;
    } else if (codon === "AGA") {
        return AminoAcid.R;
    } else if (codon === "GGA") {
        return AminoAcid.G;
    } else if (codon === "UGG") {
        return AminoAcid.W;
    } else if (codon === "CGG") {
        return AminoAcid.R;
    } else if (codon === "AGG") {
        return AminoAcid.R;
    } else if (codon === "GGG") {
        return AminoAcid.G;
    }

    throw new Error(`Invalid codon ${codon}`);
}

export function convertToString(aa: AminoAcid): string {
    if (aa === AminoAcid.A) {
        return "A";
    } else if (aa === AminoAcid.C) {
        return "C";
    } else if (aa === AminoAcid.D) {
        return "D";
    } else if (aa === AminoAcid.E) {
        return "E";
    } else if (aa === AminoAcid.F) {
        return "F";
    } else if (aa === AminoAcid.G) {
        return "G";
    } else if (aa === AminoAcid.H) {
        return "H";
    } else if (aa === AminoAcid.I) {
        return "I";
    } else if (aa === AminoAcid.K) {
        return "K";
    } else if (aa === AminoAcid.L) {
        return "L";
    } else if (aa === AminoAcid.M) {
        return "M";
    } else if (aa === AminoAcid.N) {
        return "N";
    } else if (aa === AminoAcid.P) {
        return "P";
    } else if (aa === AminoAcid.Q) {
        return "Q";
    } else if (aa === AminoAcid.R) {
        return "R";
    } else if (aa === AminoAcid.S) {
        return "S";
    } else if (aa === AminoAcid.T) {
        return "T";
    } else if (aa === AminoAcid.V) {
        return "V";
    } else if (aa === AminoAcid.W) {
        return "W";
    } else if (aa === AminoAcid.Y) {
        return "Y";
    } else if (aa === AminoAcid.Stop) {
        return "M";
    }

    throw new Error(`Amino acid code ${aa}`);
}

export function prot() {
    enum Stage { Start, Codons }
    let currentStatus = Stage.Start;
    let leftover = "";
    return new stream.Transform({
        transform(chunk, _, callback) {
            let data: string = chunk.toString();
            do {
                let i = 0;
                switch (currentStatus) {
                    case Stage.Start:
                        for (; i < data.length - 2; ++i) {
                            if (convertToAminoAcid(data.slice(i, i + 3)) === AminoAcid.M) {
                                currentStatus = Stage.Codons;
                                break;
                            }
                        }

                        break;

                    case Stage.Codons:
                        if (leftover) {
                            data = leftover + data;
                            leftover = "";
                        }

                        for (; i < data.length - 2; i += 3) {
                            const nextThree = data.slice(i, i + 3);
                            this.push(convertToString(convertToAminoAcid(nextThree)));
                        }

                        if (i !== data.length) {
                            leftover = data.slice(i - 3);
                        }

                        callback();
                        return;
                }
            } while (true);
        },
    });
}
