/*
 * Line By Line
 *
 * A NodeJS module that helps you reading large text files, line by line,
 * without buffering the files into memory.
 *
 * Copyright (c) 2012 Markus von der Wehd <mvdw@mwin.de>
 * MIT License, see LICENSE.txt, see http://www.opensource.org/licenses/mit-license.php
 *
 * Ported to TypeScript. Adapted from https://github.com/Osterjour/line-by-line
 */

// tslint:disable: only-arrow-functions
// tslint:disable: variable-name

import stream = require("stream");
import sd = require("string_decoder");
import path = require("path");
import fs = require("fs");
import events = require("events");
const StringDecoder = sd.StringDecoder;

export interface ICreateReadStreamOptions {
    flags?: string | undefined;
    encoding?: string | undefined;
    fd?: number | undefined;
    mode?: number | undefined;
    autoClose?: boolean | undefined;
    start?: number | undefined;
    end?: number | undefined;
    highWaterMark?: number | undefined;
}

export class LineByLineReader extends events.EventEmitter {
    _encoding: string;
    _readStream: stream.Readable | null;
    _filepath: string | null = null;
    _streamOptions: ICreateReadStreamOptions = {};
    _skipEmptyLines: boolean;
    _lines: string[];
    _lineFragment: string;
    _paused: boolean;
    _end: boolean;
    _ended: boolean;
    decoder: sd.NodeStringDecoder;

    constructor(
        filepath: string | stream.Readable,
        options: ICreateReadStreamOptions | null = null,
        skipEmptyLines: boolean = false) {

        super();
        const self = this;

        this._encoding = options && options.encoding || "utf8";
        if (filepath instanceof stream.Readable) {
            this._readStream = filepath;
        } else {
            this._readStream = null;
            this._filepath = path.normalize(filepath);
            this._streamOptions = { encoding: this._encoding };

            if (options && options.start) {
                this._streamOptions.start = options.start;
            }

            if (options && options.end) {
                this._streamOptions.end = options.end;
            }
        }
        this._skipEmptyLines = skipEmptyLines || false;

        this._lines = [];
        this._lineFragment = "";
        this._paused = false;
        this._end = false;
        this._ended = false;
        this.decoder = new StringDecoder(this._encoding);

        events.EventEmitter.call(this);

        setImmediate(function() {
            self._initStream();
        });
    }

    _initStream() {
        const self = this;
        if (!this._readStream && !this._filepath) {
            throw new Error("Filepath or read stream invalid");
        }

        const readStream = this._readStream
            ? this._readStream
            : fs.createReadStream(this._filepath as string, this._streamOptions);

        readStream.on("error", function(err) {
            self.emit("error", err);
        });

        readStream.on("open", function() {
            self.emit("open");
        });

        readStream.on("data", function(data) {
            self._readStream!.pause();
            let dataAsString = data;
            if (data instanceof Buffer) {
                dataAsString = self.decoder.write(data);
            }
            self._lines = self._lines.concat(dataAsString.split(/(?:\n|\r\n|\r)/g));

            self._lines[0] = self._lineFragment + self._lines[0];
            self._lineFragment = self._lines.pop() || "";

            setImmediate(function() {
                self._nextLine();
            });
        });

        readStream.on("end", function() {
            self._end = true;

            setImmediate(function() {
                self._nextLine();
            });
        });

        this._readStream = readStream;
    }

    _nextLine() {
        const self = this;
        let line: string | undefined;

        if (this._paused) {
            return;
        }

        if (this._lines.length === 0) {
            if (this._end) {
                if (this._lineFragment) {
                    this.emit("line", this._lineFragment);
                    this._lineFragment = "";
                }
                if (!this._paused) {
                    this.end();
                }
            } else {
                this._readStream!.resume();
            }
            return;
        }

        line = this._lines.shift();

        if (!this._skipEmptyLines || (line && line.length > 0)) {
            this.emit("line", line);
        }

        setImmediate(function() {
            if (!self._paused) {
                self._nextLine();
            }
        });
    }

    pause() {
        this._paused = true;
    }

    resume() {
        const self = this;

        this._paused = false;

        setImmediate(function() {
            self._nextLine();
        });
    }

    end() {
        if (!this._ended) {
            this._ended = true;
            this.emit("end");
        }
    }

    close() {
        const self = this;

        this._readStream!.destroy();
        this._end = true;
        this._lines = [];

        setImmediate(function() {
            self._nextLine();
        });
    }
}
