import System.Environment (getArgs)

-- |
-- >>> swap [1,2,3,4,5,6,7,8,9] [(0,8)]
-- [9,2,3,4,5,6,7,8,1]
--
-- >>> swap [1,2,3,4,5,6,7,8,9,10] [(0,1),(1,3)]
-- [2,4,3,1,5,6,7,8,9,10]
--
swap :: [a] -> [(Int, Int)] -> [a]
swap xs [] = xs
swap xs ((i,j):ss) = swap xs' ss where
  xs' = map replace $ zip [0..length xs -1] xs
  replace (k, x)
    | k == i    = xs !! j
    | k == j    = xs !! i
    | otherwise = x

-- |
-- >>> parse "1 2 3 4 5 6 7 8 9 : 0-8"
-- ([1,2,3,4,5,6,7,8,9],[(0,8)])
--
-- >>> parse"1 2 3 4 5 6 7 8 9 10 : 0-1, 1-3"
-- ([1,2,3,4,5,6,7,8,9,10],[(0,1),(1,3)])
--
parse :: String -> ([Int], [(Int,Int)])
parse str = (xs, ps) where
  (xs', (_:ps')) = break (==":") $ words str
  xs = map (\s -> read s :: Int) xs'
  ps = map parse' ps'
  parse' ss = (i, j) where
    [i, j] = map (\s -> read s :: Int) $ words $ map (\c -> if c `elem` "-," then ' ' else c) ss

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . uncurry swap . parse) $ lines doc where
    format xs = unwords $ map show xs

