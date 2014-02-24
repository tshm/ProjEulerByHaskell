module P14 where
import Data.Array

-- The following iterative sequence is defined for the set of positive integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1) 
-- contains 10 terms. Although it has not been proved yet (Collatz Problem),
-- it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.
--
-- |
-- >>> collatz 13
-- [13,40,20,10,5,16,8,4,2,1]
--
-- >>> findMax (collatzLen 1000000) 1000000

findMax arr n = let
  val = foldr (\i (k,v) -> if (arr!i) > v then (i,arr!i) else (k,v)) (1,1) [1..n]
  in val


collatzLen n = r where
  r = array (1, 2*n) [(k, len k) | k <- [1..2*n]]
  len 1 = 1
  len k = 1 + (lookup (next k)) where
    next k
      | k `mod` 2 == 0  = k `quot` 2
      | otherwise       = 3 * k + 1
  lookup k
    | k < n     = r ! k
    | otherwise = len k

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : (collatz $ next n) where
  next n
    | n `mod` 2 == 0  = n `quot` 2
    | otherwise       = 3 * n + 1

