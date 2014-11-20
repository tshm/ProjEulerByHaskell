import System.Environment (getArgs)

-- |
-- >>> bubble "36 47 78 28 20 79 87 16 8 45 72 69 81 66 60 8 3 86 90 90 | 1"
-- "36 47 28 20 78 79 16 8 45 72 69 81 66 60 8 3 86 87 90 90"
--
-- >>> bubble "40 69 52 42 24 16 66 | 2"
-- "40 42 24 16 52 66 69"
--
-- >>> bubble "54 46 0 34 15 48 47 53 25 18 50 5 21 76 62 48 74 1 43 74 78 29 | 6"
-- "0 15 25 18 34 5 21 46 47 48 48 1 43 50 53 29 54 62 74 74 76 78"
--
-- >>> bubble "48 51 5 61 18 | 2"
-- "5 48 18 51 61"
--
-- >>> bubble "59 68 55 31 73 4 1 25 26 19 60 0 | 2"
-- "55 31 59 4 1 25 26 19 60 0 68 73"
--
parse :: String -> ([Int], Integer)
parse str = (xs, n) where
  (wxs, _:wn) = span (/="|") $ words str
  xs = map (read :: String -> Int) wxs
  n = read (head wn) :: Integer

format :: [Int] -> String
format xs = unwords $ map show xs

bubble :: String -> String
bubble str = format xs' where
  (xs, n) = parse str
  xs' = bubble'' xs n
  bubble'' ys n'
    | n' == 0    = ys
    | ys == ys'  = ys
    | otherwise  = bubble'' ys' (n'-1)
    where
      ys' = bubble' ys

bubble' :: [Int] -> [Int]
bubble' = bubble'' [] where
  bubble'' ys []     = ys
  bubble'' ys [x]    = ys ++ [x]
  bubble'' ys (x:xs) 
    | x > head xs  = bubble'' (ys ++ [head xs]) (x:tail xs)
    | otherwise    = bubble'' (ys ++ [x])       xs

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . bubble) $ lines doc

