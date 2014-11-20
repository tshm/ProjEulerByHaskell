import System.Environment (getArgs)

-- |
-- >>> lock 3 1
-- 2
--
-- >>> lock 100 100
-- 50
--
lock :: Int -> Int -> Int
lock n m = count doors' where
  doors' = lock' m doors
  doors = take n $ repeat True
  count = foldl (\s b -> if b then s+1 else s) 0
  lock' 1 xs = (take (n-1) xs) ++ [not $ last xs]
  lock' m' xs = lock' (m'-1) $ map flip3rd $ map closeEven xs' where
    closeEven (i, b) = (i, if even i then False else b)
    flip3rd  (i, b) = if i `mod` 3 == 0 then not b else b
    xs' = zip [1..n] xs

-- |
-- >>> parse "3 1"
-- (3,1)
--
parse :: String -> (Int, Int)
parse str = (n, m) where
  n:m:[] = map (\xs -> read xs :: Int) $ words str

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . uncurry lock . parse) $ lines doc

