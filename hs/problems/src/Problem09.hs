{-
Special Pythagorean triplet


A Pythagorean triplet is a set of three natural numbers, a LT b LT c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product a*b*c.
-}

module Problem09 (solve, matching) where

-- for c <- squaresTo1000
-- for b <- squaresTo1000 < b
-- for a <- squaresTo1000 < a
-- where a + b == c
squaresTo1000 :: [(Int, Int)]
squaresTo1000 = [(x, x*x) | x <- [1..1000]]

matching :: [[Int]]
matching = [[fst c, fst b, fst a] | c <- squaresTo1000,
                                    b <- takeWhile (lt c) squaresTo1000,
                                    a <- takeWhile (lt b) squaresTo1000,
                                    snd c == snd a + snd b,
                                    1000 == fst a + fst b + fst c]
           where lt b a = fst a < fst b

solve :: Integer
solve = toInteger $ product (head (take 1 matching))
