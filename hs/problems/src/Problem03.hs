module Problem03 (solve) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

-- Author: Aaron Johnson
-- Date: 2016-05-17
-- My first prime finding algorithm in Haskell!

-- <title>Largest prime factor</title>
-- <summary>
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?
-- </summary>

data PrimeState = PrimeState {
    unsieved :: U.Vector Int,
    primes :: M.Map Int Int
} deriving (Show)

-- 1) Make init -- 2 is presieved
getInitVector :: Int -> U.Vector Int
getInitVector upTo = U.generate (upTo `quot` 2 - 1) (\x -> x*2+3)

-- 2) Filter vector with a prime
primeSieve :: U.Vector Int -> Int -> Int -> (U.Vector Int, Int)
primeSieve v prime startingAt =
    let lim = U.last v
        cut = S.fromList [startingAt, startingAt + prime.. lim]
    in (U.filter (\s -> s `S.notMember` cut) v, S.findMax cut)

-- 3) Given a prime map and an unsieved vector, assume the first element is a prime,
--    sieve the vector with it and pull it into the primes list
autoSieve :: PrimeState -> PrimeState
autoSieve state
    | U.length u == 0 = PrimeState { primes = ps, unsieved = u }
    | otherwise =
        let p       = U.head u
            (psieved, biggest) = primeSieve u p p
        in autoSieve PrimeState { unsieved = psieved, primes = M.insert p biggest ps }
    where ps = primes state
          u = unsieved state

getPrimesUnder :: Int -> [Int]
getPrimesUnder num =
    let init = getInitVector num
        pkeys = M.keys $ primes (autoSieve PrimeState { primes = M.singleton 2 2, unsieved = init })
    in pkeys

this `evenlyDivides` that = that `mod` this == 0

-- todo: to solve the problem, getting primes under 300 billion is not necessary; make the primes lazy
getPrimeFactors :: Int -> [Int]
getPrimeFactors number =
    let primes = getPrimesUnder (number `quot` 2 + 1)
        divide (v, p, agg, primes)
            | p `evenlyDivides` v  = (v `quot` p, p, p : agg, primes)
            | L.length primes == 1 && length agg == 0 = (1, v, [v], primes)
            | otherwise            = (v, L.head primes, agg, tail primes)
        stop = (\(v, _, _, _) -> v <= 1)
        result = until stop divide (number, L.head primes, [], L.tail primes)
    in (\(_, _, v, _) -> v) result

solve :: Integer
solve = 23