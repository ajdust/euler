{-
Largest palindrome product

A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers.
-}

module Problem04 (solve, isPalindrome) where

isPalindrome :: String -> Bool
isPalindrome s =
    let half = length s `quot` 2
    in all (==True) $ zipWith (==) (take half s) (take half $ reverse s)

solve :: Integer
solve = maximum [x*y | x <- [101..999], y <- [101..999], isPalindrome (show $ x*y)]

