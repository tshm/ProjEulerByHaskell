import System.Environment (getArgs)
import Data.List
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> minDis [20, 30, 40]
-- 20
--
-- >>> minDis [3, 3, 5, 7]
-- 6
--
minDis :: [Int] -> Int
minDis xs = cost where
  xp = round (realToFrac (sum xs) / (genericLength xs) :: Double)
  cost = foldl (\s x -> s + if x > xp then (x - xp) else (xp - x)) 0 xs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . minDis . map (\w -> read w :: Int) . tail . words) $ lines doc

