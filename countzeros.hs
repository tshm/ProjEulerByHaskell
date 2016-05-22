import System.Environment (getArgs)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- |
-- >>> parse "1 8"
-- (1,8)
--
parse :: String -> (Int, Int)
parse str = (x, y) where
  [x,y] = map (read :: String -> Int) $ words str

-- |
-- >>> countMatch 1 8
-- 3
--
-- >>> countMatch 2 4
-- 1
--
countMatch :: Int -> Int -> Int
countMatch x y = length (filter (\s -> x == countZeros s) bstrs) where
  bstrs = map toBin [1..y]
  toBin n = showIntAtBase 2 intToDigit n ""
  countZeros str = length $ filter (=='0') str


-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . uncurry countMatch . parse) $ lines doc

