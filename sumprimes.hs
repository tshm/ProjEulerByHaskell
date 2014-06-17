primes :: [Int]
primes =
  2 : [ n | n <- [3,5..],
    let div' x =  n `mod` x /= 0,
    all div' $ [3,5..floor $ sqrt (fromIntegral n :: Double)]
  ]

main :: IO ()
main = print $ sum $ take 1000 primes

