import stream = require("stream");
import { StreamCombiner, lineByLine } from "./util";

export function findSubstrs(str: string, substr: string) {
    let pos = 0;
    let i = str.indexOf(substr);
    const indexes: number[] = [];
    while (i !== -1) {
        indexes.push(i);
        pos = i + 1;
        i = str.indexOf(substr, pos);
    }

    return indexes;
}

export function subs() {
    const lines: string[] = [];
    const grabTwoLines = new stream.Transform({
        transform(chunk, _, callback) {
            lines.push(chunk.toString().trim());
            callback();
        },
        flush(callback) {
            const [dna, motif] = lines;
            const indexes = findSubstrs(dna, motif);
            this.push(indexes.map((v) => v + 1).join(" "));
            callback();
        },
    });

    return new StreamCombiner(lineByLine(), grabTwoLines);
}
