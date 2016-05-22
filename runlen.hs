import System.Environment (getArgs)

-- |
--
-- >>> rle "40 40 40 40 29 29 29 29 29 29 29 29 57 57 92 92 92 92 92 86 86 86 86 86 86 86 86 86 86"
-- "4 40 8 29 2 57 5 92 10 86"
--
-- >>> rle "73 73 73 73 41 41 41 41 41 41 41 41 41 41"
-- "4 73 10 41"
--
-- >>> rle "1 1 3 3 3 2 2 2 2 14 14 14 11 11 11 2"
-- "2 1 3 3 4 2 3 14 3 11 1 2"
--
-- >>> rle "7"
-- "1 7"
--
rle :: String -> String
rle str = drop 1 $ ss ++ " " ++ show nn ++ " " ++ dd where
  (ss, (dd,nn))= foldl takeDup ("", ("", 0::Int)) $ words str
  takeDup (s, (num, n)) num' 
    | num == num'  = (s, (num, n+1))
    | n == 0       = ("", (num', 1))
    | otherwise    = (s ++ " " ++ show n ++ " " ++ num, (num', 1))

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . rle) $ lines doc

