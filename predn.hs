import System.Environment (getArgs)
--import Data.List (sortBy,transpose,intercalate)

-- |
-- >>> rnk 5
-- 2
--
rnk :: Int -> Int
rnk 0 = 0
rnk 1 = 0
rnk n = if q < 2 then q else 1 + rnk q where
  q = div n 2

-- |
-- >>> map predn [0..7]
-- [0,1,1,2,1,2,2,0]
--
-- >>> predn 101
-- 1
--
-- >>> predn 25684
-- 0
--
predn :: Int -> Int
predn 0 = 0
predn n = (1 + predn r) `mod` 3 where
  p = rnk n
  r = n `mod` (2 ^ p)

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = print . predn . (read :: String -> Int)

