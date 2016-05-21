module Main where
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
-- >>> (collatzLen 13) ! 13
-- 10
--
-- >> maximumBy snd $ assocs $ (collatzLen 1000000)
-- >>> findMax (collatzLen 100)
-- (97,119)
--
-- 837799

nextSeq :: Integer -> Integer
nextSeq k
  | even k     = k `quot` 2
  | otherwise  = 3 * k + 1

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : (collatz $ nextSeq n)

collatzLen :: Integer -> Array Integer Integer
collatzLen n = r where
  r = array (1, n) [(k, len k) | k <- [1..n]]
  len k
    | k == 1     = 1
    | k' < n     = 1 + r ! k'
    | otherwise  = 1 + len k'
    where k' = nextSeq k

findMax :: Array Integer Integer -> (Integer, Integer)
findMax arr = foldl1 max' $ assocs arr where
  max' x y = if snd x > snd y then x else y

main :: IO ()
main = print $ "(index, value) = " ++ show (findMax (collatzLen 1000000))
