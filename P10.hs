module P10 where

primes =
  2 : [ n | n <- [3,5..],
    let div x =  n `mod` x /= 0,
    all div $ [3,5..(floor $ sqrt $ fromInteger n)]
  ]

main = do
  print $ foldr (+) 0 (takeWhile (< 2 * 10^6) primes)

