import stream = require("stream");
import { StreamCombiner, fasta, lineByLine, IFastaChunk } from "./util";

export function cons() {
    const countA: number[] = [];
    const countC: number[] = [];
    const countG: number[] = [];
    const countT: number[] = [];

    const and = new stream.Transform({
        writableObjectMode: true,
        transform(chunk: IFastaChunk, _, callback) {
            const dna = chunk.content;
            for (let i = 0; i < dna.length; ++i) {
                const c = dna.charAt(i);
                countA[i] = (countA[i] || 0) + (c === "A" ? 1 : 0);
                countC[i] = (countC[i] || 0) + (c === "C" ? 1 : 0);
                countG[i] = (countG[i] || 0) + (c === "G" ? 1 : 0);
                countT[i] = (countT[i] || 0) + (c === "T" ? 1 : 0);
            }
            callback();
        },
        flush(callback) {
            const maxCs: string[] = [];
            for (let i = 0; i < countA.length; ++i) {
                let max = countA[i];
                let maxC = "A";
                if (countC[i] > max) {
                    max = countC[i];
                    maxC = "C";
                }
                if (countG[i] > max) {
                    max = countG[i];
                    maxC = "G";
                }
                if (countT[i] > max) {
                    max = countT[i];
                    maxC = "T";
                }

                maxCs.push(maxC);
            }

            this.push(maxCs.join("") + "\n");
            this.push(`A: ${countA.join(" ")}\n`);
            this.push(`C: ${countC.join(" ")}\n`);
            this.push(`G: ${countG.join(" ")}\n`);
            this.push(`T: ${countT.join(" ")}\n`);
            callback();
        },
    });

    return new StreamCombiner(lineByLine(), fasta(), and);
}
