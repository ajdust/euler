module Problem02 (solve) where

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

solve :: Integer
solve = sum $ filter even $ takeWhile (<(round 4e6)) fib