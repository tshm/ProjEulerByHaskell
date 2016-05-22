import System.Environment (getArgs)
--import Data.List (filter)

-- |
-- >>> black ["John","Sara","Tom","Susan"] 3
-- "Sara"
--
-- >>> black ["John","Tom","Mary"] 5
-- "Mary"
--
black :: [String] -> Int -> String

black [x] _ = x
black xs n = black xs' n where
  n' = n `mod` length xs
  xs' = if n' == 0
      then init xs
      else take (n'-1) xs ++ drop n' xs

-- |
-- >>> parse "John Sara Tom Susan | 3"
-- (["John","Sara","Tom","Susan"],3)
--
-- >>> parse "John Tom Mary | 5"
-- (["John","Tom","Mary"],5)
--
parse :: String -> ([String], Int)
parse str = (names, read num :: Int) where
  (names, [_,num]) = break (=="|") $ words str

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . uncurry black . parse) $ lines doc

