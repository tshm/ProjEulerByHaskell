import System.Environment (getArgs)

-- |
-- >>> multl "9 0 6 | 15 14 9"
-- "135 0 54"
--
-- >>> multl "5 | 8"
-- "40"
--
-- >>> multl "13 4 15 1 15 5 | 1 4 15 14 8 2"
-- "13 16 225 14 120 10"
--
multl :: String -> String
multl str = unwords $ map (show) zs where
  (xs, (_:ys)) = span (/="|") $ words str
  zs = zipWith (\x y -> (read x :: Int) * (read y :: Int)) xs ys

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . multl) $ lines doc

