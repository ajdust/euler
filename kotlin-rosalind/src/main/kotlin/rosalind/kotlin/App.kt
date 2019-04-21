package rosalind.kotlin

import java.io.File

fun main(args: Array<String>) {
	if (args.count() < 1) {
		println("Please provide a filename as input")
        return;
    }

	var A = 0
	var C = 0
	var G = 0
	var T = 0
    File(args[0]).forEachLine { line ->
    	line.forEach {
    		when (it) {
    			'A' -> A += 1
    			'C' -> C += 1
    			'G' -> G += 1
    			'T' -> T += 1
    		}
    	}
    }

    println("$A $C $G $T")
}
