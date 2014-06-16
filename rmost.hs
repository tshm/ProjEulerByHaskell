import Data.List
import System.Environment

-- | find right most pos
-- >>> rmost "Hello World,r"
-- 8
-- >>> rmost ""
--
rmost :: String -> Int
rmost str = pos where
  (l,_:c:[]) = span (/=',') str
  indices = elemIndices c l
  pos = if null indices then -1 else last $ indices

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . rmost) $ filter (not . null) $ lines doc where

