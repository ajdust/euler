module Problem03 (solve) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Set as S

-- Author: Aaron Johnson
-- Date: 2016-05-17
-- My first prime finding algorithm in Haskell!

-- <title>Largest prime factor</title>
-- <summary>
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?
-- </summary>

data PrimeState = PrimeState {
    primeIndex :: Int,
    primeVector :: U.Vector Int
} deriving (Show)

-- 1) Make init -- 2 is presieved
getInitVector :: Int -> U.Vector Int
getInitVector upTo = U.generate (upTo `quot` 2 - 1) (\x -> x*2+3)

-- 2) Filter vector with a prime
primeSieve :: U.Vector Int -> Int -> U.Vector Int
primeSieve v prime =
    let lim = U.last v
        cut = S.fromList [prime*2, prime*3.. lim]
    in U.filter (\s -> s `S.notMember` cut) v

-- 3) Given an index and a vector, sieve the vector with the value at the index
autoSieve :: PrimeState -> PrimeState
autoSieve state
    | pindex >= vlen = PrimeState { primeIndex = vlen, primeVector = v }
    | otherwise =
        let p = v U.! pindex
            sieved = primeSieve v p
        in autoSieve (PrimeState { primeIndex = pindex + 1, primeVector = sieved})
    where v = primeVector state
          vlen = U.length v
          pindex = primeIndex state

getPrimesUnder :: Int -> U.Vector Int
getPrimesUnder num =
    let init = getInitVector num
        primes = primeVector (autoSieve PrimeState { primeIndex = 0, primeVector = init })
    in U.cons 2 primes

this `evenlyDivides` that = that `mod` this == 0

-- todo: to solve the problem, getting primes under 300 billion is not necessary; make the primes lazy
getPrimeFactors :: Int -> [Int]
getPrimeFactors number =
    let primes = getPrimesUnder (number `quot` 2 + 1)
        divider (v, p, agg, primes)
            | p `evenlyDivides` v  = (v `quot` p, p, p : agg, primes)
            | U.length primes == 1 && length agg == 0 = (1, v, [v], primes)
            | otherwise            = (v, U.head primes, agg, U.tail primes)
        stop = (\(v, _, _, _) -> v <= 1)
        result = until stop divider (number, U.head primes, [], U.tail primes)
    in (\(_, _, v, _) -> v) result

solve :: Integer
solve = 23