import System.Environment (getArgs)

-- |
-- >>> guessn "100 Lower Lower Higher Lower Lower Lower Yay!"
-- 13
--
-- >>> guessn "948 Higher Lower Yay!"
-- 593
--
guessn :: String -> Int
guessn ul = ans where
  (n:xs) = words ul
  (ans, _) = foldl branch (0, read n :: Int) xs
  branch (l,u) s
    | head s == 'L'  = (l, g - 1)
    | head s == 'H'  = (g + 1, u)
    | otherwise      = (g, g)
    where g = (l + u + 1) `div` 2

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . guessn) $ lines doc

