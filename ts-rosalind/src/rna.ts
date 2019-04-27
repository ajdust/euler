import stream = require("stream");

export function rna(): stream.Transform {

    return new stream.Transform({
        transform(chunk, encoding, callback) {
            const data = chunk.toString();
            for (let i = 0; i < data.length; ++i) {
                const c = data.charAt(i);
                if (c === "T") {
                    this.push("U");
                } else {
                    this.push(c);
                }
            }

            callback();
        },
    });
}
