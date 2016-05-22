module Main where
import Data.List
import Data.Array
-- |
-- >>> fib 12
-- 144 
--
-- The 12th term is the first term to contain three digits.
-- >>> find ((3 ==) . digits . fib) [0..]
-- Just 12

fib :: Integer -> Integer
fib n = r ! n where
  r = array (0, n) [(k,f k) | k <- [0..n]]
  f 0 = 0
  f 1 = 1
  f m = r ! (m-1) + r ! (m-2)

dumbfib :: Integer -> Integer
dumbfib n = fib' n where
  fib' 0 = 0
  fib' 1 = 1
  fib' m = fib' (m-1) + fib' (m-2)

digits :: Integer -> Int
digits = (length . show)

-- What is the first term in the Fibonacci sequence to contain 1000 digits?
main :: IO ()
main = print $ findIndex ((1000 ==) . digits . fib) [0..]
