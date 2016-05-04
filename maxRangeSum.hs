import System.Environment (getArgs)
-- import Data.List (maximumBy)
-- import Data.Ord (comparing)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> maxRangeSum 5 [7, -3, -10, 4, 2, 8, -2, 4, -5, -2]
-- 16
--
-- >>> uncurry maxRangeSum (parse "6;-4 3 -10 5 3 -7 -3 7 -6 3")
-- 0
--
-- >>> uncurry maxRangeSum (parse "3;-7 0 -45 34 -24 7")
-- 17
-- 
maxRangeSum :: Int -> [Int] -> Int
maxRangeSum n xs = max 0 s where
  rangeSum xs' = sum $ take n xs' 
  s = maximum $ map (\i -> rangeSum $ drop i xs) [0..(length xs - n)]

-- |
-- >>> parse "6;-4 3 -10 5 3 -7 -3 7 -6 3"
-- (6,[-4,3,-10,5,3,-7,-3,7,-6,3])
--
parse :: String -> (Int, [Int])
parse xs = (n, ys) where
  xs' = map (\c -> if c `elem` [';',' '] then ',' else c) xs
  (n:ys) = read $ "[" ++ xs' ++ "]"

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . uncurry maxRangeSum . parse) $ lines doc

