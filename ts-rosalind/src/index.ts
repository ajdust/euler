import { LineByLineReader } from "./LineByLineReader";

interface NucleotideCount {
    A: number;
    C: number;
    G: number;
    T: number;
}

function dna(input: LineByLineReader, output: (message: string) => void) {
    let A = 0, C = 0, T = 0, G = 0;

    input.on("line", (line: string) => {
        for (let i = 0; i < line.length; ++i) {
            const c = line.charAt(i);
            if (c === "A") {
                A += 1;
            } else if (c === "C") {
                C += 1;
            } else if (c === "T") {
                T += 1;
            } else if (c === "G") {
                G += 1;
            }
        }
    });

    input.on("end", () => {
        output(`${A} ${C} ${G} ${T}`);
    });

}

export function main(args: string[]) {
    const problem = args.length >= 3 ? args[2] : "dna";
    const filepath = args.length >= 4 ? args[3] : null;
    if (problem === "dna") {
        dna(new LineByLineReader(filepath || "./rosalind_dna.txt"), console.log);
    } else {
        console.error(`Unrecognized problem '${problem}' `);
    }
}

main(process.argv);
