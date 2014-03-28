module Main where
import Data.List
-- |
-- >>> getMaxPrimes 1 41
-- 40
--
-- >>> getMaxPrimes (-79) 1601
-- 80

findAB :: Integer -> String
findAB n = "seqlen: " ++ show v ++ ", a: " ++ show a ++ ", b: " ++ show b where
  (a, b, v) = maximumBy mm [(a', b', getMaxPrimes a' b') | a' <- [-n..n], b' <- [-n..n]]
  mm (_, _, v1) (_, _, v2) = compare v1 v2

getMaxPrimes :: Integer -> Integer -> Int
getMaxPrimes a b = maxIndex $ findIndex (not . isPrime) (series a b) where
  maxIndex (Just x) = x
  maxIndex Nothing = 0
  series a' b' = [n * n + a' * n + b' | n <- [0..100]]

isPrime :: Integer -> Bool
isPrime x = x == head [p | p<-primes, p>=x]

primes :: [Integer]
primes =
  2 : [ n | n <- [3,5..],
    let div' x =  n `mod` x /= 0,
    all div' $ [3,5..(floor $ sqrt $ (fromInteger n :: Double))]
  ]

main :: IO ()
main = print $ findAB 1000

