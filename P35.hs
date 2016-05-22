module Main where
-- |
-- >>> isCircularPrime 197
-- True
--
-- >>> filter isCircularPrime $ takeWhile (<100) primes
-- [2,3,5,7,11,13,17,31,37,71,73,79,97]
isCircularPrime :: Int -> Bool
isCircularPrime n = all isPrime $ circularNumbers n

-- |
-- >>> circularNumbers 197
-- [197,971,719]
--
-- >>> circularNumbers 3
-- [3]
circularNumbers :: Int -> [Int]
circularNumbers n = map cyc [0..l - 1] where
  cyc i = (read :: String -> Int) $ take l $ drop i (d ++ d)
  l = length d
  d = show n

isPrime :: Int -> Bool
isPrime x = x == head [p | p<-primes, p>=x]

primes :: [Int]
primes =
  2 : [ n | n <- [3,5..],
    let div' x =  n `mod` x /= 0,
    all div' $ [3,5..floor $ sqrt (fromIntegral n :: Double)]
  ]

main :: IO ()
main = do
  print $ length $ filter isCircularPrime $ takeWhile (<1000000) primes

