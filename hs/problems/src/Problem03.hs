module Problem03 (solve) where

-- Author: Aaron Johnson
-- Date: 2016-05-17
-- My first prime finding algorithm in Haskell!

-- <title>Largest prime factor</title>
-- <summary>
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?
-- </summary>


get_lastsieve siever = fst siever
get_prime siever = snd siever

-- given a target, t, and a cur_value and a step, step until you reach or pass the target
get_next target lastsieve prime = until (>=target) (+prime) lastsieve

get_next_siever target siever =
	let lastsieve = get_lastsieve siever
	    prime = get_prime siever
	in (get_next target lastsieve prime, prime)

get_next_sievers target sievers = map (\sieve -> get_next_siever target sieve) sievers

all_exceed_target target sievers = all (>target) (map get_lastsieve sievers)

get_target_sieves target sievers =
	let targetsievers = get_next_sievers target sievers
	    is_prime = all_exceed_target target targetsievers
	in if is_prime then (target, target) : targetsievers else targetsievers

get_target_sieves_and_next_target sievers_and_target =
	let sievers = fst sievers_and_target
	    target = snd sievers_and_target
	in (get_target_sieves target sievers, target + 2)

get_until_new_primes_count target sievers count_bigger_by =
	let count = (length sievers) + count_bigger_by
	in until (\x -> length (fst x) > count) get_target_sieves_and_next_target (sievers, target)

get_until_new_primes_bigger target sievers bigger_by =
	until (\x -> (get_prime $ head (fst x)) > bigger_by) get_target_sieves_and_next_target (sievers, target)

extract_primes sievers_and_target = map snd (fst (sievers_and_target))

get_primes number =
	if number <= 5 then reverse $ take number [11, 7, 5, 3, 2]
	else let result = get_until_new_primes_count 13 [(11,11),(7,7),(5,5),(3,3),(2,2)] (number - 5)
		 in extract_primes result

get_primes_under number =
	let result = get_until_new_primes_bigger 13 [(11,11),(7,7),(5,5),(3,3),(2,2)] (number - 5)
	in extract_primes result

this `evenly_divides` that = that `mod` this == 0

get_prime_factors :: Integral a => a -> [a]
get_prime_factors number =
	let primes = get_primes_under (number `quot` 2)
	    stop = (\(current_value, _, _, _) -> current_value <= 1)
	    divider (v, _, [], []) = (1, v, [v], [])
	    divider (cur_value, cur_prime, aggregator, primes) =
	    	if cur_prime `evenly_divides` cur_value then (cur_value `quot` cur_prime, cur_prime, cur_prime : aggregator, primes)
	    	else (cur_value, head primes, aggregator, tail primes)
	    result = until stop divider (number, head primes, [], tail primes)
	in (\(_, _, v, _) -> v) result

solve :: Integer
solve = 23