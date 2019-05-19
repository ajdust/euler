{-
10001st prime

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10001st prime number?
-}

module Problem07 (solve) where
import Problem03 (primes)

solve :: Integer
solve = toInteger (primes !! 10000)
