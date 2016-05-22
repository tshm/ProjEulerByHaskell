module Main where
-- |
-- >>> primes !! 5
-- 13

primes :: [Integer]
primes =
  [ n | n <- [2..],
    let div' x = n `mod` x /= 0,
    let max' = floor . sqrt $ (fromInteger n :: Double),
    all div' [2..max']
  ]

main :: IO ()
main = print $ primes !! 10000
