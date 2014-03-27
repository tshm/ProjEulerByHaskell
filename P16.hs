module Main where

-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- What is the sum of the digits of the number 2^1000?
--
-- |
-- >>> dumb 15
-- 26

dumb :: Integer -> Integer
dumb n = sum $ map (\c -> read [c] :: Integer) str where
  str = show (2^n :: Integer)

main :: IO ()
main = print $ dumb 1000

