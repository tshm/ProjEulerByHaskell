import Debug.Trace
import Data.List
import System.Environment

-- | parse input line
-- >>> parse "20,21,22;45,46,47"
-- ([20,21,22],[45,46,47])
--
parse :: String -> ([Int],[Int])
parse line = read formatted where
  formatted = ("([" ++ left ++ "],[" ++ right ++ "])")
  (left,_:right) = span (/=';') line

getIntersect :: ([Int],[Int]) -> [Int]
getIntersect (left, right) = intersect left right

showArray :: Show a => [a] -> String
showArray = tail . init . show

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . showArray . getIntersect . parse) $ lines doc where

