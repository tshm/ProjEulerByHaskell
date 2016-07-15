import System.Environment (getArgs)

-- |
-- >>> remc "hello world" "def"
-- "hllo worl"
--
-- >>> remc "how are you" "abc"
-- "how re you"
--
remc :: String -> String -> String
remc str xs = foldr remc' "" str where
  remc' c s = if elem c xs then s else c : s

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = putStrLn . uncurry remc . split
    split s = (a, tail b) where
      (a:b:[]) = lines $ map (\c -> if c == ',' then '\n' else c) s

