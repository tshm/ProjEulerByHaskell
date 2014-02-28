module P25 where
import Data.List
import Data.Array
-- |
-- >>> fib 12
-- 144 
--
-- The 12th term is the first term to contain three digits.
-- >>> findIndex (\x -> 3 == digits x) [ fib n | n<-[0..] ]
-- Just 12
--
-- What is the first term in the Fibonacci sequence to contain 1000 digits?
-- >>> findIndex (\x -> 1000 == digits x) [ fib n | n<-[0..] ]

fib n = r ! n where
  r = array (0, n) [(k,f k) | k <- [0..n]]
  f 0 = 0
  f 1 = 1
  f n = r ! (n-1) + r ! (n-2)

dumbfib n = fib n where
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib (n-2)

digits n = length $ show n

