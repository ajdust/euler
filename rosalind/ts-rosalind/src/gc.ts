import stream = require("stream");
import { StreamCombiner, fasta, lineByLine, maxBy, IFastaChunk } from "./util";
import { countNucleotidesSync, INucleotideCount } from "./dna";

export interface ICountPerFasta {
    fastaChunk: IFastaChunk;
    nucleotideCount: INucleotideCount;
}

export function countPerFasta(): stream.Transform {

    return new stream.Transform({
        writableObjectMode: true,
        readableObjectMode: true,
        transform(fastaChunk: IFastaChunk, _, callback) {
            const nucleotideCount = countNucleotidesSync(fastaChunk.content);
            this.push({ fastaChunk, nucleotideCount });
            callback();
        },
    });
}

export interface IGCPerFasta {
    fastaChunk: IFastaChunk;
    gc: number;
}

export function gcPerFasta(): stream.Transform {

    return new stream.Transform({
        writableObjectMode: true,
        readableObjectMode: true,
        transform(chunk: ICountPerFasta, _, callback) {
            const nc = chunk.nucleotideCount;
            this.push({
                fastaChunk: chunk.fastaChunk,
                gc: (nc.G + nc.C) / (nc.G + nc.C + nc.T + nc.A),
            });

            callback();
        },
    });
}

export function gcToString(): stream.Transform {

    return new stream.Transform({
        writableObjectMode: true,
        transform(chunk: IGCPerFasta, _, callback) {
            this.push(`${chunk.fastaChunk.label}\n${chunk.gc}`);
            callback();
        },
    });
}

export function gc(): stream.Transform {

    const combiner = new StreamCombiner(
        lineByLine(),
        fasta(),
        countPerFasta(),
        gcPerFasta(),
        maxBy((chunk: IGCPerFasta) => chunk.gc),
        gcToString(),
    );

    return combiner;
}
