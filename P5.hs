module Main where
-- |
-- >>> lcm' 10
-- 2520

lcm' :: Integer -> Integer
lcm' 2 = 2
lcm' n = lcm n (lcm' (n-1))
    
main :: IO ()
main = print $ lcm' 20

