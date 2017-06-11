/**
 * Created by Aaron Johnson on 2017-06-10
 */

package problems

import kotlin.coroutines.experimental.*
import kotlin.collections.*

object Sequences {

    val fibonacci: Sequence<Int> = buildSequence {
        var a = 0
        var b = 1
        yield(1)
        while (true) {
            yield(a + b)
            val tmp = a + b
            a = b
            b = tmp
        }
    }

    val primes: Sequence<Long>  = buildSequence {
        val composites: MutableMap<Long, Long> = mutableMapOf()
        var n: Long = 3
        yield(2L)

        while (true) {
            var prime = composites[n]
            while (prime != null) {
                // remove composite
                composites.remove(n)

                // add next composite for that prime
                var composite = n + prime + prime
                while (composites.contains(composite)) {
                    composite += prime + prime
                }
                composites[composite] = prime

                n += 2
                prime = composites.get(n)
            }

            composites[n * n] = n
            yield(n)
            n += 2
        }
    }

    fun primeFactors(of: Long): MutableList<Long> {
        val factors: MutableList<Long> = mutableListOf()
        var quotient = of
        for (prime in primes) {
            if (prime > quotient) {
                break;
            }

            var remainder = quotient % prime
            while (remainder == 0L) {
                quotient /= prime
                remainder = quotient % prime
                factors.add(prime)
            }
        }

        return factors
    }
}
