import scala.collection._

object facben {

  class PrimeGenerator {
    var n: Long = 3L
    var last: Long = 2L
    val sieve: mutable.LongMap[Long] = mutable.LongMap.empty[Long]

    def next(): Long = {
      var prime = 0L
      while ({ prime = sieve.getOrElse(n, 0); prime != 0L }) {
        sieve.remove(n)

        var composite: Long = n + prime + prime
        while (sieve.contains(composite)) {
          composite += prime + prime
        }

        sieve += (composite, prime)
        n += 2L
      }

      sieve += (n * n, n)
      val r = last
      last = n
      n += 2L
      r
    }
  }

  class FactorFinder {
    val known: mutable.LongMap[mutable.Set[Long]] = initKnown()
    val nextPrimes: PrimeGenerator = new PrimeGenerator()
    val knownPrimes: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.empty[Long]

    def initKnown(): mutable.LongMap[mutable.Set[Long]] = {
      var lm = mutable.LongMap.empty[mutable.Set[Long]]
      var s = mutable.Set.empty[Long]
      s += 1L
      lm += (1L, s)
      lm
    }

    def getPrimeFactors(of: Long): mutable.ArrayBuffer[Long] = {
      var factors = mutable.ArrayBuffer.empty[Long]
      var quotient = of

      val kp = knownPrimes.iterator
      var prime = 0L
      while (kp.hasNext && { prime = kp.next(); true } && prime <= quotient) {

        var remainder = quotient % prime
        while (remainder == 0L) {
          quotient /= prime
          remainder = quotient % prime
          factors += prime
        }
      }

      prime = nextPrimes.next()
      knownPrimes += prime
      while (prime <= quotient) {

        var remainder = quotient % prime
        while (remainder == 0L) {
          quotient /= prime
          remainder = quotient % prime
          factors += prime
        }

        prime = nextPrimes.next()
        knownPrimes += prime
      }

      factors
    }

    def getFactors(of: Long): mutable.Set[Long] = {
      known.get(of) match {
        case Some(facs) => facs
        case _ => {
          val primeFactors = getPrimeFactors(of)
          var factors = mutable.HashSet.empty[Long]
          factors += 1L
          factors += of
          for (prime <- primeFactors) {
            val factor = of / prime
            for (subfactor <- getFactors(factor)) {
              factors += subfactor
            }
          }

          known += (of, factors)
          factors
        }
      }
    }
  }

  def solve(): Long = {

    val finder = new FactorFinder()
    var adder = 0L
    var tn = 0L
    var length = 0

    while (length < 1000) {
      adder += 1L
      tn += adder

      val factors = finder.getFactors(tn)
      length = factors.size
    }

    tn
  }

  def main(args: Array[String]): Unit = {
    val answer = solve()
    println(s"Answer: $answer")
  }
}

