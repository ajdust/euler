import stream = require("stream");
import { StreamCombiner, reverse } from "./util";

export function complement() {
    return new stream.Transform({
        transform(chunk, _, callback) {
            const data: string = chunk.toString();
            for (let i = 0; i < data.length; ++i) {
                const c = data.charAt(i);
                if (c === "A") {
                    this.push("T");
                } else if (c === "C") {
                    this.push("G");
                } else if (c === "G") {
                    this.push("C");
                } else if (c === "T") {
                    this.push("A");
                }
            }
            callback();
        },
    });
}

export function revc() {
    return new StreamCombiner(complement(), reverse());
}
