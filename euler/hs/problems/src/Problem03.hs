-- Author: Aaron Johnson
-- Date: 2016-09-05

{-
Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
-}

module Problem03 (solve, primes, primeFactors) where
import qualified Data.Map as Map

primes :: [Int]
primes =
    prime_ 2 Map.empty
    where
        addNextComposite :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
        addNextComposite n prime sieve
            | Map.member n sieve = addNextComposite (n + prime) prime sieve
            | otherwise          = Map.insert n prime sieve
        prime_ :: Int -> Map.Map Int Int -> [Int]
        prime_ n sieve = case Map.lookup n sieve of
            Just prime ->
                let gc = Map.delete n sieve
                    ac = addNextComposite (n + prime) prime gc
                in prime_ (n + 1) ac
            Nothing    ->
                let ac = Map.insert (n*n) n sieve
                in n : prime_ (n + 1) ac

primeFactors :: Int -> [Int]
primeFactors n =
    pfactor primes n
    where
        pfactor :: [Int] -> Int -> [Int]
        pfactor [] _ = error "Empty prime list"
        pfactor ps@(prime:pt) n
            | prime >= n         = [prime]
            | n `mod` prime == 0 = prime : pfactor ps (n `quot` prime)
            | otherwise          = pfactor pt n

solve :: Integer
solve = toInteger (maximum $ primeFactors 600851475143)