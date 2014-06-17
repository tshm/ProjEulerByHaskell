primes :: [Int]
primes =
  2 : [ n | n <- [3,5..],
    let div' x =  n `mod` x /= 0,
    all div' $ [3,5..floor $ sqrt (fromIntegral n :: Double)]
  ]

isPalindrome :: Int -> Bool
isPalindrome n = str == reverse str where
  str = show n

main :: IO ()
main = print $ last $ filter isPalindrome $ takeWhile (<1000) primes

