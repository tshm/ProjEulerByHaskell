module P27 where
import Data.List
-- |
-- >>> getMaxPrimes 1 41
-- 40
--
-- >>> getMaxPrimes (-79) 1601
-- 80
--
-- >>> findAB 1000

findAB n = "seqlen:" ++ show v ++ ", a:" ++ show a ++ ", b:" ++ show b where
  (a,b,v) = maximumBy mm [(a, b, getMaxPrimes a b) | a <- [-n..n], b <- [-n..n]]
  mm (a1, b1, v1) (a2, b2, v2) = compare v1 v2

getMaxPrimes a b = maxIndex $ findIndex (not . isPrime) (series a b) where
  maxIndex (Just x) = x
  maxIndex Nothing = 0
  series a b = [n^2 + a * n + b | n <- [0..100]]

isPrime x = x == head [p | p<-primes, p>=x]

primes =
  2 : [ n | n <- [3,5..],
    let div x =  n `mod` x /= 0,
    all div $ [3,5..(floor $ sqrt $ fromInteger n)]
  ]
