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
    pulled :: [Int],
    primes :: M.Map Int Int
} deriving (Show)

together :: [a] -> [a] -> [a]
together a b = foldr (:) b a

-- Get a vector of numbers without multiples of 2. Use a vector length and multiple.
getInitVector :: Int -> Int -> U.Vector Int
getInitVector upTo i = U.generate upTo (\x -> (i*upTo+x)*2+3)

-- Filter a vector with a prime, starting at a specific multiple.
primeSieve :: U.Vector Int -> Int -> Int -> (U.Vector Int, Int)
primeSieve v prime startingAt =
    let lim = U.last v
        cut = S.fromList [startingAt, startingAt + prime ..lim]
    in (U.filter (\s -> s `S.notMember` cut) v, S.findMax cut)

-- Filter a vector with a prime-starter-multiple map, get back the sieved vector and the updated prime-starter-multiple map
preSieve :: U.Vector Int -> M.Map Int Int -> (U.Vector Int, M.Map Int Int)
preSieve toSieve sieveWith =
    let getRange k a = [a, a+k ..(U.last toSieve)]
        ranges = M.mapWithKey getRange sieveWith
        newMap = M.map (\x -> last x) ranges
        cut = foldl1 S.union (map S.fromList (M.elems ranges))
    in (U.filter (\s -> s `S.notMember` cut) toSieve, newMap)

-- Given a PrimeState, assume the first element is prime and recursively sieve the vector
autoSieve :: PrimeState -> PrimeState
autoSieve state
    | U.length u == 0 = PrimeState { primes = ps, unsieved = u, pulled = puls }
    | otherwise =
        let p       = U.head u
            (psieved, biggest) = primeSieve u p p
        in autoSieve PrimeState { unsieved = psieved, primes = M.insert p biggest ps, pulled = p:puls }
    where ps = primes state
          u = unsieved state
          puls = pulled state

getPrimesUnder :: Int -> [Int]
getPrimesUnder num =
    let init = getInitVector (num `quot` 2 - 1) 0
        pkeys = M.keys $ primes (autoSieve PrimeState { primes = M.singleton 2 2, unsieved = init, pulled = [] })
    in pkeys

-- something like::::
getPrimes :: [Int]
getPrimes =
    let getMore i state =
            let puls = pulled state
                ps = primes state
                (presieved, psUpdated) = preSieve (getInitVector 1000 i) ps
                newState = autoSieve PrimeState { primes = psUpdated, unsieved = presieved, pulled = [] }
            in together (reverse puls) (getMore (i + 1) newState)
    in 2 : getMore 1 (autoSieve PrimeState { primes = M.empty, unsieved = getInitVector 1000 0, pulled = [] })
    -- initial : more

this `evenlyDivides` that = that `mod` this == 0

getPrimeFactors :: Int -> [Int]
getPrimeFactors number =
    let divide n p
            | n == p = ([n], 1)
            | n == 1 = ([], 1)
            | p `evenlyDivides` n =
                let divided = n `quot` p
                    (factors, num) = divide divided p
                in (p : factors, num)
            | otherwise = ([], n)
        divideFold (prevFactors, curNum) nextPrime
            | curNum == 1 = ([], 0)
            | otherwise = (together newFactors prevFactors, nextNum)
            where (newFactors, nextNum) = divide curNum nextPrime
        primeScan = scanl divideFold ([], number) getPrimes
    in fst $ last (takeWhile (\x -> snd x /= 0) primeScan)

solve :: Int
solve = head $ getPrimeFactors 600851475143