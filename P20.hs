module P20 where

ans n = foldr1 (+) $ map (\c -> read [c] :: Int) $ show $ fact n where
  fact n = foldr1 (*) [1..n]

main = do
  print $ ans 100
