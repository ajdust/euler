import { dna } from "./dna";
import { rna } from "./rna";
import fs = require("fs");
import stream = require("stream");
import { endl, lineByLine } from "./util";
import { gc } from "./gc";

export function main(args: string[]) {
    const problem = args.length >= 3 ? args[2] : "gc";
    const filePath = args.length >= 4 ? args[3] : null;

    try {
        let transform: stream.Transform;
        if (problem === "dna") {
            transform = fs.createReadStream(filePath || "./rosalind_dna.txt").pipe(dna());
        } else if (problem === "rna") {
            transform = fs.createReadStream(filePath || "./rosalind_rna.txt").pipe(rna());
        } else if (problem === "gc") {
            transform = fs.createReadStream(filePath || "./rosalind_gc.txt").pipe(gc());
        } else {
            throw new Error(`Unrecognized problem '${problem}'`);
        }

        transform.pipe(endl()).pipe(process.stdout);
    } catch (exception) {
        console.error(exception);
    }
}

main(process.argv);
