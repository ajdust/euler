/**
  * Created by aaron on 8/14/2016.
  */

package Problems

/** Solve Euler Problem 1: Multiples of 3 and 5
  *
  * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6
  * and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5
  * below 1000.
  *
  * This can be solved more efficiently with a formula.
  *
  * The sum of the multiples of numbers can be given a formula. The set of multiples
  * of 3 and 5 intersect at the multiples of 15. So the solution is the sum of the
  * multiples of 3 added to the sum of the multiples of 5 minus the sum of the
  * multiples of 15.
  *
  * The sum of natural numbers from 1 to n = n(n+1)/2
  * The sum of the first n multiples of 3  = 3n(n+1)/2
  * The sum of the first n multiples of 5  = 5n(n+1)/2
  * The sum of the first n multiples of 15 = 15n(n+1)/2
  *
  * Simple integer division can find the size of n for a number at or under k and multiple m:
  *     n = k / m
  *
  * Hence, given a limit of multiples under k, the sum S for this problem can be computed:
  *     S = 3(k/3)((k/3)+1)/2 + 5(k/5)((k/5)+1)/2 - 15(k/15)((k/15)+1)/2
  *
  * With k = 999 this equates to 233168.
  */
class Problem001 extends Problem {
  private def bruteSolve(limit : Int) =
    (for (n <- 1 to limit if n % 3 == 0 || n % 5 == 0) yield n).sum

  private def quickSolve(k : Int) =
    3*(k/3)*((k/3) + 1)/2 + 5*(k/5)*((k/5)+1)/2 - 15*(k/15)*((k/15)+1)/2

  def solve = {
    bruteSolve(999).toString
  }
}
