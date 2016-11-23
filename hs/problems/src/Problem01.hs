{-
Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6
and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5
below 1000.
-}

module Problem01 (solve) where

-- ran into an interesting operator issue
-- 3*n3*(n3+1)/2 + 5*n5*(n5+1)/2 - 15*n15*(n15+1)/2
-- without parenthesis around the numerator - say in (n)/2 - will be incorrect
-- C# and Python did not have this issue with their infix operators
smartSolve :: Integer -> Integer
smartSolve n =
    let (/) = quot in
        let n3 = n / 3
            n5 = n / 5
            n15 = n / 15
        in (3*n3*(n3+1))/2 + (5*n5*(n5+1))/2 - (15*n15*(n15+1))/2

bruteSolve :: Integer -> Integer
bruteSolve n = sum [ x | x <- [1 .. n], x `mod` 3 == 0 || x `mod` 5 == 0]

solve = smartSolve 999