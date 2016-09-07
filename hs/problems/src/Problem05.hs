module Problem05 (solve, countNumbers, commonPrimeFactors) where
import Problem03 (primeFactors)

-- Smallest multiple
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


-- To solve this problem, we need the number with the minimum set of
-- prime factors necessary to create each number from 1 to 20.

import qualified Data.Map as Map

-- This function is probably generic and built in.
countNumbers :: [Int] -> Map.Map Int Int
countNumbers ns =
    countNumbers_ ns Map.empty
    where
        countNumbers_ :: [Int] -> Map.Map Int Int -> Map.Map Int Int
        countNumbers_ [] m = m
        countNumbers_ (x:xs) m = case Map.lookup x m of
            Just n -> countNumbers_ xs (Map.insert x (n + 1) m)
            Nothing -> countNumbers_ xs (Map.insert x 1 m)

countPrimeFactors = countNumbers . primeFactors

commonPrimeFactors :: [Int] -> Map.Map Int Int
commonPrimeFactors [] = Map.empty
commonPrimeFactors ns =
    foldl1 mergePrimeFactors eachPrimeFactors
    where eachPrimeFactors = map countPrimeFactors ns
          mergePrimeFactors pf1 pf2 =
            Map.mergeWithKey (\k a b -> Just $ max a b) id id pf1 pf2

solve :: Integer
solve = toInteger (Map.foldlWithKey (\a k b -> a * k^b) 1 $ commonPrimeFactors [2..20])