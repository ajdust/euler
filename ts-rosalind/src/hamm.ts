import stream = require("stream");

export function hammCount(left: string, right: string): number {
    let count = 0;
    for (let i = 0; i < left.length && i < right.length; ++i) {
        if (left.charAt(i) !== right.charAt(i)) {
            count += 1;
        }
    }

    return count;
}

export function hamm(): stream.Transform {
    let all = "";
    return new stream.Transform({
        transform(chunk, _, callback) {
            all += chunk.toString();
            callback();
        },
        flush(callback) {
            const [line1, line2] = all.split("\n");
            this.push(hammCount(line1, line2).toString());
            callback();
        },
    });
}
