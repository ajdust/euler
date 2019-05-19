/**
  * Created by aaron on 8/14/2016.
  */

package problems

import scala.collection._

object Sequences {

  private val fibStream = Stream.iterate((0, 1))(t => (t._2, t._1 + t._2))
  def fibonacci: Stream[Int] = fibStream.map(x => x._1)

  private val primesStream: Stream[(Long, mutable.LongMap[Long])] = Stream.iterate((3L, mutable.LongMap.empty[Long]))(t => {
    var (n, composites) = t
    while (composites.contains(n)) {
      val prime = composites(n)
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

    composites += (n*n -> n)
    (n + 2, composites)
  })

  def primes: Stream[Long] = Stream(2L) ++ primesStream.drop(1).map(x => x._1 - 2)

  def primeFactors(n: Long): mutable.ArrayBuffer[Long] = {
    var factors = mutable.ArrayBuffer.empty[Long]
    val primesIterator = primes.iterator
    var prime = primesIterator.next
    var quotient = n
    while (prime <= quotient) {
      var remainder = quotient % prime
      while (remainder == 0) {
        quotient = quotient / prime
        remainder = quotient % prime
        factors += prime
      }
      prime = primesIterator.next
    }
    factors
  }

  private val triangleStream = Stream.iterate((2L, 1L))(t => (t._1 + 1L, t._1 + t._2))
  def triangleNumbers: Stream[Long] = triangleStream.map(x => x._2)
}

