import System.Environment (getArgs)

-- |
-- >>> parse "72 64 150 | 100 18 33 | 13 250 -6"
-- [[72,64,150],[100,18,33],[13,250,-6]]
--
parse :: String -> [[Int]]
parse str = map toInts $ lines $ map (\c -> if c == '|' then '\n' else c) str
  where
    toInts xs = map (\s -> read s :: Int) $ words xs

-- |
-- >>> getMax [[72,64,150],[100,18,33],[13,250,-6]]
-- [100,250,150]
--
getMax :: [[Int]] -> [Int]
getMax = foldr1 (zipWith max)

format :: [Int] -> String
format xs = unwords $ map show xs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . getMax . parse) $ lines doc
