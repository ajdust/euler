import fs = require("fs");
import stream = require("stream");
import { endl, streamFromString } from "./util";
import { dna } from "./dna";
import { rna } from "./rna";
import { gc } from "./gc";
import { hamm } from "./hamm";
import { iprb } from "./iprb";
import { fib } from "./fib";
import { revc } from "./revc";
import { prot } from "./prot";
import { subs } from "./subs";
import { cons } from "./cons";

export function main(args: string[]) {
    const problem = args.length >= 3 ? args[2] : "cons";
    const filePath = args.length >= 4 ? args[3] : null;

    try {
        let transform: stream.Readable;
        if (problem === "dna") {
            transform = fs.createReadStream(filePath || "./rosalind_dna.txt").pipe(dna());
        } else if (problem === "rna") {
            transform = fs.createReadStream(filePath || "./rosalind_rna.txt").pipe(rna());
        } else if (problem === "gc") {
            transform = fs.createReadStream(filePath || "./rosalind_gc.txt").pipe(gc());
        } else if (problem === "hamm") {
            transform = fs.createReadStream(filePath || "./rosalind_hamm.txt").pipe(hamm());
        } else if (problem === "iprb") {
            transform = streamFromString(iprb(20, 18, 24).toString());
        } else if (problem === "revc") {
            transform = fs.createReadStream(filePath || "./rosalind_revc.txt").pipe(revc());
        } else if (problem === "fib") {
            transform = streamFromString(fib(33, 4).toString());
        } else if (problem === "prot") {
            transform = fs.createReadStream(filePath || "./rosalind_prot.txt").pipe(prot());
        } else if (problem === "subs") {
            transform = fs.createReadStream(filePath || "./rosalind_subs.txt").pipe(subs());
        } else if (problem === "cons") {
            transform = fs.createReadStream(filePath || "./rosalind_cons.txt").pipe(cons());
        } else {
            throw new Error(`Unrecognized problem '${problem}'`);
        }

        transform.pipe(endl()).pipe(process.stdout);
    } catch (exception) {
        console.error(exception);
    }
}

main(process.argv);
