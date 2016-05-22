module Main where

pythagoreanTriplet :: [(Integer, Integer, Integer)]
pythagoreanTriplet = [(a,b,c) |
  a <- [1..999], b <- [1..999],
  let c = 1000 - b - a,
  a^(2 :: Integer) + b^(2 :: Integer) == c^(2 :: Integer)]

main :: IO ()
main = print $ let (a,b,c) = head pythagoreanTriplet in a * b * c

