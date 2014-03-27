module Main where

-- | test cont
-- >>> take 5 primes
-- [2,3,5,7,11]

primes :: [Integer]
primes =
  2 : [ n | n <- [3,5..],
    let divide x =  n `mod` x /= 0,
    all divide $ [3,5..(floor $ sqrt $ (fromInteger n :: Double))]
  ]

main :: IO ()
main = do
  print $ foldr (+) 0 (takeWhile (< 2 * 10^(6 :: Integer)) primes)

