module P7 where
-- |
-- >>> primes !! 5
-- 13
--
-- >>> primes !! 10000
--

primes =
  [ n | n <- [2..],
    let div x = n `mod` x /= 0,
    let max = floor . sqrt $ fromInteger n,
    all div [2..max]
  ]

