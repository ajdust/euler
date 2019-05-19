import stream = require("stream");

// ported from https://gist.github.com/nicolashery/5910969
export class StreamCombiner extends stream.PassThrough {
    streams: stream.Writable[];
    writeStream: stream.Writable | undefined;

    constructor(...streams: stream.Writable[]) {
        super();

        if (!streams.length) {
            throw new Error("No streams passed to StreamCombiner");
        }

        this.streams = streams;

        // When a source stream is piped to us, undo that pipe,
        // and pipe it into our stream collection
        const self = this;
        this.on("pipe", (source: stream.Readable) => {
            source.unpipe(self);

            // pipe source into first transformable stream
            let writeStream = source.pipe(self.streams[0]);

            // chain the rest of the pipes
            for (let i = 1; i < self.streams.length; ++i) {
                writeStream = writeStream.pipe(streams[i]);
            }

            self.writeStream = writeStream;
        });
    }

    // forward pipe to inner pipe
    pipe(dest: any, options: any) {
        if (this.writeStream) {
            return this.writeStream.pipe(dest, options);
        }
    }
}

export function maxBy(predicate: (element: any) => number) {
    let element = {};
    let maxSize = Number.MIN_VALUE;
    return new stream.Transform({
        writableObjectMode: true,
        readableObjectMode: true,
        transform(chunk, _, callback) {
            const size = predicate(chunk);
            if (size > maxSize) {
                maxSize = size;
                element = chunk;
            }

            callback();
        },
        flush(this: stream.Transform, callback) {
            this.push(element);
            callback();
        },
    });
}

export function endl(): stream.Transform {
    return new stream.Transform({
        transform(this: stream.Transform, chunk, _, callback) {
            this.push(chunk);
            callback();
        },
        flush(callback) {
            this.push("\n");
            callback();
        },
    });
}

export function lineByLine(): stream.Transform {

    const chars: string[] = [];
    return new stream.Transform({
        transform(this: stream.Transform, chunk, _, callback) {
            const data: string = chunk.toString();
            for (let i = 0; i < data.length; ++i) {
                const c = data.charAt(i);
                chars.push(c);
                if (c === "\n") {
                    if (chars.length) {
                        this.push(chars.join(""));
                    }
                    chars.length = 0;
                }
            }
            callback();
        },
        flush(this: stream.Transform, callback) {
            if (chars.length) {
                this.push(chars.join(""));
            }

            callback();
        },
    });
}

export interface IFastaChunk {
    label: string;
    content: string;
}

// expects line-by-line source stream
export function fasta(): stream.Transform {

    let fastaChunk: IFastaChunk = { label: "", content: "" };
    return new stream.Transform({
        readableObjectMode: true,
        transform(this: stream.Transform, chunk, _, callback) {
            if (!chunk.length) {
                callback();
                return;
            }

            const data: string = chunk.toString();
            if (data.charAt(0) === ">") {
                if (fastaChunk.label.length) {
                    fastaChunk.content = fastaChunk.content.trim();
                    this.push(fastaChunk);
                    fastaChunk = { label: "", content: "" };
                }

                fastaChunk.label = data.slice(1).trimRight();
            } else {
                fastaChunk.content += data;
            }

            callback();
        },
        flush(this: stream.Transform, callback) {
            if (fastaChunk.label.length) {
                fastaChunk.content = fastaChunk.content.trim();
                this.push(fastaChunk);
            }

            callback();
        },
    });
}

export function reverse() {
    const buffer: string[] = [];
    return new stream.Transform({
        transform(chunk, _, callback) {
            buffer.push(chunk.toString());
            callback();
        },
        flush(callback) {
            this.push(buffer.map((v) => v.split("").reverse().join(""))
                .reverse().join(""));
            callback();
        },
    });
}

export function streamFromString(s: string): stream.Readable {
    const pass = new stream.Readable();
    pass.push(s);
    pass.push(null);
    return pass;
}
