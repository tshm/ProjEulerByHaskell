import System.Environment (getArgs)
--import Data.List (partition, permutations)
--import Data.Maybe (mapMaybe)
--import Debug.Trace (trace)
--import Control.Applicative (liftA2)

-- |
-- >>> parse "3x2 | 2 1"
-- ((3,2),(2,1))
--
parse :: String -> ((Int,Int), (Int,Int))
parse str = ((w,h), (x,y)) where
  (w:h:_:x:y:_) = map parseInt $ words $ map removeX str
  removeX c = if c=='x' then ' ' else c
  parseInt s = if s == "|" then 0 else read s :: Int

-- |
-- >>> walk (3,2) (2,1)
-- 5
--
-- >>> walk (4,4) (3,3)
-- 14
--
walk :: (Int,Int) -> (Int,Int) -> Int
walk (w,h) (x,y)
  | y == h     = x
  | otherwise  = w + walk (h-1, w) (h-y, x)

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = print . uncurry walk . parse

