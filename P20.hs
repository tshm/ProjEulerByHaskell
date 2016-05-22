module Main where

ans :: Integer -> Integer
ans n = foldr1 (+) $ map (\c -> read [c] :: Integer) $ show $ fact n where
  fact m = foldr1 (*) [1..m]

main :: IO ()
main = print $ ans 100

