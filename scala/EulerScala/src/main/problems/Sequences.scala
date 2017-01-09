/**
  * Created by aaron on 8/14/2016.
  */
package problems

import scala.collection._

object Sequences {

  private def fibonacciTuple = Stream.iterate((0,1))(t => (t._2, t._1 + t._2))

  def fibonacci: Stream[Int] = fibonacciTuple.map(x => x._1)

  private val primesStart: Tuple2[Long, mutable.LongMap[Long]] = (3, mutable.LongMap.empty)
  private def primesTuple = Stream.iterate(primesStart)(t => {

    var (n, composites) = t
    while (composites.contains(n)) {
      composites.get(n) match {
        case Some(prime) => {
          // remove composite
          composites.remove(n)

          // add next composite for that prime
          var check = n + prime + prime
          while (composites.contains(check)) {
            check += prime + prime
          }
          composites += (check, prime)

          n += 2
        }
        case _ => ()
      }
    }

    composites += (n*n -> n)
    n += 2
    (n, composites)

  })

  def primes: Stream[Long] = Stream(2L) ++ primesTuple.drop(1).map(x => x._1 - 2)

  def primeFactors(n: Long): Stream[Long] = {
    def pfactor(primes: Stream[Long], n: Long): Stream[Long] = {
      primes match {
        case Stream() => throw new Exception("Empty prime list")
        case prime #:: pt => {
          if (prime >= n) {
            Stream(prime)
          } else if (n % prime == 0) {
            Stream(prime) ++ pfactor(primes, n / prime)
          } else {
            pfactor(pt, n)
          }
        }
      }
    }

    pfactor(primes, n)
  }
}

