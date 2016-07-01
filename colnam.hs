import System.Environment (getArgs)
-- import Data.List (transpose,intercalate)

-- |
-- >>> colname 27
-- "AA"
--
-- >>> colname 25
-- "Y"
--
-- >>> colname 52
-- "AZ"
--
-- >>> colname 3702
-- "ELJ"
--
colname :: Int -> String
colname 0 = []
colname n = (colname q) ++ [alph r] where
  (q, r) = quotRem (n-1) 26

-- 1  -> "A"
-- 2  -> "B"
-- ..
-- 26 -> "Z"
-- 27 -> "AA"
-- 28 -> "AB"
--
alph :: Int -> Char
alph n = ['A'..'Z'] !! n

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = putStrLn . colname . (read :: String -> Int)

