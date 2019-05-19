import stream = require("stream");
import { StreamCombiner } from "./util";

export interface INucleotideCount {
    A: number;
    C: number;
    T: number;
    G: number;
}

export function countNucleotidesSync(data: string): INucleotideCount {
    let A = 0, C = 0, T = 0, G = 0;
    for (let i = 0; i < data.length; ++i) {
        const c = data.charAt(i);
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

    return { A, C, T, G };
}

export function countNucleotides(): stream.Transform {

    let A = 0, C = 0, T = 0, G = 0;

    return new stream.Transform({
        readableObjectMode: true,
        transform(chunk, _, callback) {
            const data: string = chunk.toString();
            for (let i = 0; i < data.length; ++i) {
                const c = data.charAt(i);
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

            callback();
        },
        flush(this: stream.Transform, callback) {
            this.push({ A, C, T, G });
            callback();
        },
    });
}

export function stringifyNucleotideCount(): stream.Transform {
    return new stream.Transform({
        writableObjectMode: true,
        transform(chunk: INucleotideCount, _, callback) {
            this.push(`${chunk.A} ${chunk.C} ${chunk.T} ${chunk.G}`);
            callback();
        },
    });
}

export function dna(): stream.Transform {
    const sc = new StreamCombiner(countNucleotides(), stringifyNucleotideCount());
    return sc;
}
