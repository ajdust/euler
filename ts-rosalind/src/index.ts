import fs = require("fs");
import stream = require("stream");

interface NucleotideCount {
    A: number;
    C: number;
    G: number;
    T: number;
}

function endl(): stream.Transform {
    return new stream.Transform({
        transform(this: stream.Transform, chunk, encoding, callback) {
            this.push(chunk);
            callback();
        },
        flush(callback) {
            this.push("\n");
            callback();
        },
    });
}

function dna(): stream.Transform {

    let A = 0, C = 0, T = 0, G = 0;

    return new stream.Transform({
        transform(chunk, encoding, callback) {
            const data = chunk.toString();
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
            this.push(`${A} ${C} ${G} ${T}`);
            callback();
        },
    });
}

export function main(args: string[]) {
    const problem = args.length >= 3 ? args[2] : "dna";
    const filepath = args.length >= 4 ? args[3] : null;

    try {
        let transform: stream.Transform;
        if (problem === "dna") {
            transform = fs.createReadStream(filepath || "./rosalind_dna.txt").pipe(dna());
        } else {
            throw new Error(`Unrecognized problem '${problem}'`);
        }

        transform.pipe(endl()).pipe(process.stdout);
    } catch (exception) {
        console.error(exception);
    }
}

main(process.argv);
