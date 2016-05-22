import System.Environment (getArgs)
import Data.List

-- | matrot
-- >>> matrot "a b c d"
-- "c a d b"
--
-- >>> matrot "a b c d e f g h i"
-- "g d a h e b i f c"
--
-- >>> matrot "a b c d e f g h i j k l m n o p"
-- "m i e a n j f b o k g c p l h d"
--
matrot :: String -> String
matrot s = unwords arr' where
  arr = words s
  n = size $ length arr
  row1 = map (\i -> n * (n - i)) [1..n]
  ixs = concatMap (\i -> map (+i) row1) [0..n-1]
  arr' = map (arr !!) ixs

-- |
-- >>> size 81
-- 9
--
-- >>> size 9
-- 3
--
size :: Int -> Int
size n = m + 1 where
  Just m = elemIndex n [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . matrot) $ lines doc

